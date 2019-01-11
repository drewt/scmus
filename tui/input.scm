;;
;; Copyright 2014-2018 Drew Thoreson
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.
;;

(module scmus.tui.input (key-case
                         mouse-case
                         <text-input>
                         text-input-text
                         text-input-saved-text
                         text-input-cursor-pos
                         text-input-prefix
                         text-input-stolen-focus
                         text-input-editing?
                         text-input-on-commit
                         text-input-on-cancel
                         text-input-on-begin
                         text-input-on-leave
                         make-text-input
                         text-input-get-text
                         text-input-set-text!
                         text-input-length
                         text-input-get-cursor-pos
                         text-input-set-cursor-pos!
                         text-input-move!
                         text-input-insert!
                         text-input-backspace!
                         text-input-delete!
                         text-input-commit
                         text-input-cancel
                         text-input-begin
                         get-key
                         do-handle-input
                         enable-mouse
                         mouse-input?)
  (import coops
          drewt.ncurses
          scmus.base
          scmus.tui.widget
          scmus.tui.misc)
  (reexport (only drewt.ncurses
              mouse-event-id
              mouse-event-x
              mouse-event-y
              mouse-event-z
              mouse-event-bstate
              KEY_MOUSE
              BUTTON1_PRESSED
              BUTTON1_RELEASED
              BUTTON1_CLICKED
              BUTTON1_DOUBLE_CLICKED
              BUTTON1_TRIPLE_CLICKED
              BUTTON2_PRESSED
              BUTTON2_RELEASED
              BUTTON2_CLICKED
              BUTTON2_DOUBLE_CLICKED
              BUTTON2_TRIPLE_CLICKED
              BUTTON3_PRESSED
              BUTTON3_RELEASED
              BUTTON3_CLICKED
              BUTTON3_DOUBLE_CLICKED
              BUTTON3_TRIPLE_CLICKED
              BUTTON4_PRESSED
              BUTTON4_RELEASED
              BUTTON4_CLICKED
              BUTTON4_DOUBLE_CLICKED
              BUTTON4_TRIPLE_CLICKED
              BUTTON_SHIFT
              BUTTON_CTRL
              BUTTON_ALT
              ALL_MOUSE_EVENTS
              REPORT_MOUSE_POSITION))
  ; XXX: If we don't have NCURSES_MOUSE_VERSION > 1,
  ;      then we export BUTTON5_* as zero.  That way
  ;      KEY-CASE always fails to get a BUTTON5_* code.
  (cond-expand
    (ncurses-mouse-v2
      (reexport (only drewt.ncurses
                  BUTTON5_PRESSED
                  BUTTON5_RELEASED
                  BUTTON5_CLICKED
                  BUTTON5_DOUBLE_CLICKED
                  BUTTON5_TRIPLE_CLICKED)))
    (else
      (export BUTTON5_PRESSED
              BUTTON5_RELEASED
              BUTTON5_CLICKED
              BUTTON5_DOUBLE_CLICKED
              BUTTON5_TRIPLE_CLICKED)
      (define BUTTON5_PRESSED 0)
      (define BUTTON5_RELEASED 0)
      (define BUTTON5_CLICKED 0)
      (define BUTTON5_DOUBLE_CLICKED 0)
      (define BUTTON5_TRIPLE_CLICKED 0)))

  ;; FOREIGN-VALUE constants don't work in CASE expressions, so we
  ;; have to use a chain of IFs.
  (define-syntax key-case
    (syntax-rules (else)
      ((key-case key) (void))
      ((key-case key (else first rest ...))
        (begin first rest ...))
      ((key-case key ((choices ...) first rest ...) others ...)
        (if (member key (list choices ...))
          (begin first rest ...)
          (key-case key others ...)))))

  (define-syntax *mouse-case
    (syntax-rules (else)
      ((*mouse-case bstate) (void))
      ((*mouse-case bstate (else first rest ...))
        (begin first rest ...))
      ((*mouse-case bstate ((choices ...) first rest ...) others ...)
        (if (zero? (bitwise-and bstate (bitwise-ior choices ...)))
          (*mouse-case bstate others ...)
          (begin first rest ...)))))

  (define-syntax mouse-case
    (syntax-rules ()
      ((mouse-case mev choices ...)
        (*mouse-case (mouse-event-bstate mev) choices ...))))

  ;; Text widget of the form '<prefix><text>' where
  ;;   <prefix> is optional descriptive text
  ;;   <text>   is editable text
  ;; TODO: scroll when the cursor moves beyond the edge of the available area
  (define-class <text-input> (<textual>)
    ((text          initform: '()
                    accessor: text-input-text)
     (saved-text    initform: '()
                    accessor: text-input-saved-text)
     (scroll-pos    initform: 0
                    accessor: text-input-scroll-pos)
     (cursor-pos    initform: 0
                    accessor: text-input-cursor-pos)
     ; TODO: allow specifying percentage of available cols to allocate to prefix
     (prefix        initform: ""
                    accessor: text-input-prefix)
     (stolen-focus  initform: #f
                    accessor: text-input-stolen-focus)
     (editing?      initform: #f
                    accessor: text-input-editing?)
     (on-commit     initform: (lambda (_) #f)
                    accessor: text-input-on-commit)
     (on-cancel     initform: (lambda (_) #f)
                    accessor: text-input-on-cancel)
     (on-begin      initform: (lambda (_) #f)
                    accessor: text-input-on-begin)
     (on-leave      initform: (lambda (_) #f)
                    accessor: text-input-on-leave)))

  (define (make-text-input text prefix . args)
    (apply make <text-input>
                'text (reverse (string->list text))
                'prefix prefix
                args))

  (define-method (text-text (text <text-input>))
    (list (string-append (text-input-prefix text)
                         (list->string (drop (reverse (text-input-text text))
                                             (text-input-scroll-pos text))))))

  (define-method (widget-size (text <text-input>) available-cols available-rows)
    (values available-cols (min available-rows 1)))

  (define (text-input-update-cursor input)
    (let ((len  (text-input-length input))
          (cols (- (widget-cols input)
                   (string-length (text-input-prefix input)))))
      ; get cursor pos relative to scroll pos
      (define (relative-cursor-pos input)
        (- len
           (text-input-scroll-pos input)
           (text-input-cursor-pos input)))
      (define (scroll input n)
        (set! (text-input-scroll-pos input)
          (max 0 (min len (+ (text-input-scroll-pos input) n)))))
      ; scroll backward if there's empty space on the right
      (if (< (+ 1 (- len (text-input-scroll-pos input))) cols)
        (scroll input (- (+ 1 (- len (text-input-scroll-pos input)))
                         cols)))
      (let ((cursor-pos (relative-cursor-pos input)))
        (cond
          ; cursor off right: scroll forward
          ((>= cursor-pos cols)
            (scroll input (- (+ 1 cursor-pos) cols)))
          ; cursor off left: scroll backward
          ((negative? cursor-pos)
            (scroll input cursor-pos))))
      (move (widget-y input)
            (min (- (COLS) 1)
                 (+ (widget-x input)
                    (string-length (text-input-prefix input))
                    (relative-cursor-pos input))))))

  (define-method ((setter text-input-editing?) after: (widget <text-input>) editing?)
    (if editing?
      (begin (text-input-update-cursor widget)
             (curs_set 1))
      (curs_set 0))
    (widget-damaged! widget))

  (define-method ((setter text-input-cursor-pos) after: (widget <text-input>) n)
    (when (text-input-editing? widget)
      (text-input-update-cursor widget)))

  (define-method ((setter text-input-text) after: (widget <text-input>) text)
    (when (text-input-editing? widget)
      (text-input-update-cursor widget))
    (widget-damaged! widget))

  (define-method (text-input-get-text (widget <text-input>))
    (list->string (reverse (text-input-text widget))))

  (define-method (text-input-set-text! (widget <text-input>) text)
    (set! (text-input-text widget) (reverse (string->list text)))
    (set! (text-input-cursor-pos widget) 0))

  ; TODO: store length
  (define-method (text-input-length (widget <text-input>))
    (length (text-input-text widget)))

  (define-method (text-input-get-cursor-pos (widget <text-input>))
    (- (text-input-length widget)
       (text-input-cursor-pos widget)))

  (define-method (text-input-set-cursor-pos! (widget <text-input>) n)
    (set! (text-input-cursor-pos widget)
      (max 0 (- (text-input-length widget) n))))

  (define-method (handle-input (widget <text-input>) input event)
    (if (text-input-editing? widget)
      (key-case input
        ((#\newline)     (text-input-commit widget))
        ((#\escape)      (text-input-cancel widget))
        ((KEY_LEFT)      (text-input-move! widget -1))
        ((KEY_RIGHT)     (text-input-move! widget 1))
        ((KEY_HOME)      (set! (text-input-cursor-pos widget)
                               (text-input-length widget)))
        ((KEY_END)       (set! (text-input-cursor-pos widget) 0))
        ((KEY_BACKSPACE) (text-input-backspace! widget))
        ((KEY_DC)        (text-input-delete! widget))
        (else
          (when (char? input)
            (text-input-insert! widget input))))
      (call-next-method))
    (widget-damaged! widget))

  (define-method (text-input-move! (widget <text-input>) n)
    (set! (text-input-cursor-pos widget)
      (max 0 (min (text-input-length widget)
                  (- (text-input-cursor-pos widget) n)))))

  (define-method (text-input-insert! (widget <text-input>) char)
    (let ((text (text-input-text widget))
          (pos  (text-input-cursor-pos widget)))
      (set! (text-input-text widget)
        (append (take text pos)
                (cons char (drop text pos))))))

  (define-method (text-input-backspace! (widget <text-input>))
    (let ((text (text-input-text widget))
          (pos  (text-input-cursor-pos widget)))
      (when (< pos (text-input-length widget))
        (set! (text-input-text widget)
          (append (take text pos)
                  (drop text (+ pos 1)))))))

  (define-method (text-input-delete! (widget <text-input>))
    (let ((text (text-input-text widget))
          (pos  (text-input-cursor-pos widget)))
      (unless (or (= pos 0)
                  (null? text))
        (set! (text-input-cursor-pos widget)
          (- (text-input-cursor-pos widget) 1))
        (set! (text-input-text widget)
          (append (take text (- pos 1))
                  (drop text pos))))))

  (define-method (text-input-commit (widget <text-input>))
    (set! (text-input-editing? widget) #f)
    (set! (text-input-saved-text widget) '())
    ((text-input-on-commit widget) widget)
    (text-input-leave widget))

  (define-method (text-input-cancel (widget <text-input>))
    (set! (text-input-editing? widget) #f)
    (set! (text-input-text widget)
          (text-input-saved-text widget))
    (set! (text-input-saved-text widget) '())
    ((text-input-on-cancel widget) widget)
    (text-input-leave widget))

  (define-method (text-input-leave (widget <text-input>))
    (when (text-input-stolen-focus widget)
      (override-focus-end)
      (set! (text-input-stolen-focus widget) #f))
    ((text-input-on-leave widget) widget))

  (define-method (text-input-begin (widget <text-input>) #!key (steal-focus #f))
    (when steal-focus
      (set! (text-input-stolen-focus widget) #t)
      (override-focus widget))
    (set! (text-input-editing? widget) #t)
    (set! (text-input-saved-text widget)
          (text-input-text widget))
    ((text-input-on-begin widget) widget))

  (define *focus-override* #f)
  
  (define (override-focus widget)
    (assert (not *focus-override*) "override-focus" widget)
    (set! *focus-override* widget))

  (define (override-focus-end)
    (set! *focus-override* #f))

  (define waiting-for-key? #f)

  (define (get-key then #!optional whitelist)
    (set! waiting-for-key? (lambda (k)
                             (unless (and whitelist (not (memv k whitelist)))
                               (set! waiting-for-key? #f)
                               (then k)))))

  (define (do-handle-input widget input)
    (if (eqv? input KEY_MOUSE)
      (let ((mev (getmouse)))
        (handle-input (get-widget-at widget (mouse-event-x mev) (mouse-event-y mev)) input mev))
      (if waiting-for-key?
        (waiting-for-key? input)
        (handle-input (or *focus-override* (widget-focus widget)) input #f))))

  (define (enable-mouse enable?)
    (mousemask (if enable? ALL_MOUSE_EVENTS 0)))

  (define (mouse-input? mev input-type)
    (not (zero? (bitwise-and (mouse-event-bstate mev) input-type)))))
