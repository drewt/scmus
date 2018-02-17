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

(module scmus.tui.window *
  (import coops
          scmus.base
          scmus.format
          scmus.tui.display
          scmus.tui.widget)

  ;;
  ;; Window
  ;;
  ;; A window is a list with a visible section and a cursor.  The visible
  ;; section follows the cursor.
  ;;
  ;;              ,+- - - - - - - - - - - -+ <--- 0
  ;;             / |                       |
  ;;            |  +- - - - - - - - - - - -+
  ;;            |  |                       |
  ;;            |  +=======================+,<--- top-pos
  ;;            |  |                       | \
  ;;            |  +-----------------------+<-|-- sel-pos
  ;; data-len -<   |#######################|  |
  ;;            |  +-----------------------+   >- nr-lines
  ;;            |  |                       |  |
  ;;            |  +-----------------------+  |
  ;;            |  |                       | /
  ;;            |  +=======================+`
  ;;             \ |                       |
  ;;              `+- - - - - - - - - - - -+
  ;;
  ;; A window can be searched.  This works by calling the function in the @match
  ;; slot on each row of the window; if @match returns true, then the row is
  ;; considered to be a match for the query.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <window> (<widget>)
    ((data       initform: '()
                 ; writer below
                 reader:   window-data)
     (data-len   initform: 0
                 ; writer below
                 reader:   window-data-len)
     (top-pos    initform: 0
                 accessor: window-top-pos)
     (sel-pos    initform: 0
                 accessor: window-sel-pos)
     (marked     initform: '()
                 ; writer below
                 reader:   *window-marked)
     (nr-lines   initform: 0
                 ; writer below
                 reader:   window-nr-lines)
     (query      initform: ""
                 accessor: window-query) 
     (h-border   initform: 1
                 accessor: window-h-border)
     ;; Because COOPS does not play well across multiple compilation units, we
     ;; use our own OOP implementation for customizing windows.  Ideally, the
     ;; functions stored in the slots below would be defined as methods, but
     ;; then they couldn't be specialized in another compilation unit.
     (activate   initform: void
                 accessor: window-activate)
     (deactivate initform: void
                 accessor: window-deactivate)
     (match      initform: (lambda (e q) #f)
                 accessor: window-match)
     (add        initform: void
                 accessor: window-add)
     (remove     initform: void
                 accessor: window-remove)
     (clear      initform: void
                 accessor: window-clear)
     (edit       initform: void
                 accessor: window-edit)
     (move       initform: void
                 accessor: window-move)
     (cursed     initform: (win-cursed-fn (lambda (x) #f))
                 reader:   *window-cursed)
     (format     initform: #f
                 reader:   window-format)))

  (define-method (initialize-instance (window <window>))
    (call-next-method)
    (set! (window-data-len window) (length (window-data window)))
    ; allow giving a format string directly instead of a procedure
    (let ((format (window-format window)))
      (if (and format (not (procedure? format)))
        (set! (slot-value window 'format) (lambda (tag) format)))))

  (define-method ((setter window-data) (window <window>) data)
    (set! (slot-value window 'data) data)
    (set! (window-data-len window) (length (window-data window)))
    (widget-damaged! window))

  ;; Whenever the length of the window data changes, we need to make sure that
  ;; the values of top-pos and sel-pos still make sense.
  (define-method ((setter window-data-len) (window <window>) len)
    (set! (slot-value window 'data-len) len)
    (when (>= (window-sel-pos window) len)
      (set! (window-sel-pos window) (max 0 (- len 1))))
    (when (>= (window-top-pos window) len)
      (set! (window-top-pos window) (max 0 (- len 1))))
    (widget-damaged! window))

  (define-method ((setter *window-marked) (window <window>) marked)
    (set! (slot-value window 'marked) marked)
    (widget-damaged! window))

  (define-method ((setter window-nr-lines) (window <window>) nr-lines)
    (let ((top-pos (window-top-pos window))
          (sel-pos (window-sel-pos window)))
      (when (<= nr-lines (- sel-pos top-pos))
        (set! (window-top-pos window)
              (+ top-pos
                 (- (window-nr-lines window)
                    nr-lines)))))
    (set! (slot-value window 'nr-lines) nr-lines))

  (define-syntax define-subclass-predicate
    (syntax-rules ()
      ((define-subclass-predicate predname classname)
        (define (predname obj)
          (subclass? (class-of obj) classname)))))

  (define-subclass-predicate widget? <widget>)
  (define-subclass-predicate container? <container>)
  (define-subclass-predicate window? <window>)

  (define-method (widget-geometry-set! (window <window>) cols rows)
    (set! (window-nr-lines window) rows))

  (define (make-window . args)
    (apply make <window> args))

  (: window-sel-offset (window -> fixnum))
  (define (window-sel-offset window)
    (- (window-sel-pos window)
       (window-top-pos window)))

  (: window-top (window -> list))
  (define (window-top window)
    (assert (>= (length (window-data window)) (window-top-pos window)))
    (list-tail (window-data window) (window-top-pos window)))

  (: window-selected (window -> *))
  (define (window-selected window)
    (assert (> (length (window-data window)) (window-sel-pos window))
            "window-selected")
    (list-ref (window-data window) (window-sel-pos window)))

  (: window-all-selected (window -> list))
  (define (window-all-selected window)
    (define (select-from indices lst)
      (cdr (fold (lambda (x acc)
                   (if (member (car acc) indices)
                     (cons (+ (car acc) 1)
                           (cons x (cdr acc)))
                     (cons (+ (car acc) 1)
                           (cdr acc))))
                 '(0 . ())
                 lst)))
    (let* ((sel-pos (window-sel-pos window))
           (marked (window-marked window)))
      (reverse (select-from marked (window-data window)))))

  ;; XXX: selected row counts as marked
  (: window-marked (window -> list))
  (define (window-marked window)
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (if (member sel-pos marked)
        marked
        (cons sel-pos marked))))

  (: window-mark! (window -> undefined))
  (define (window-mark! window)
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (unless (or (<= (window-data-len window) 0) (member sel-pos marked))
        (set! (*window-marked window) (cons sel-pos marked)))))

  (: window-unmark! (window -> undefined))
  (define (window-unmark! window)
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (if (and (positive? (window-data-len window)) (member sel-pos marked))
        (set! (*window-marked window) (remove (lambda (x) (= x sel-pos)) marked)))))

  ;; toggles the 'marked' status of the selected row
  (: window-toggle-mark! (window -> undefined))
  (define (window-toggle-mark! window)
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (when (positive? (window-data-len window))
        (if (member sel-pos marked)
          (set! (*window-marked window) (remove (lambda (x) (= x sel-pos)) marked))
          (set! (*window-marked window) (cons sel-pos marked))))))

  (: window-clear-marked! (window -> undefined))
  (define (window-clear-marked! window)
    (set! (*window-marked window) '())
    (widget-damaged! window))

  (: window-activate! (window -> undefined))
  (define (window-activate! window)
    (when (positive? (window-data-len window))
      ((window-activate window) window)))

  (: window-deactivate! (window -> undefined))
  (define (window-deactivate! window)
    (when (positive? (window-data-len window))
      ((window-deactivate window) window)))

  (: window-add! (window -> undefined))
  (define (window-add! window)
    (when (positive? (window-data-len window))
      ((window-add window) window)))

  (: window-remove! (window -> undefined))
  (define (window-remove! window)
    (when (positive? (window-data-len window))
      ((window-remove window) window)))

  (: window-clear! (window -> undefined))
  (define (window-clear! window)
    (when (positive? (window-data-len window))
      ((window-clear window) window)))

  (: window-edit! (window -> undefined))
  (define (window-edit! window)
    (when (positive? (window-data-len window))
      ((window-edit window) window)))

  (: window-move! (window boolean -> undefined))
  (define (window-move! window before)
    (when (positive? (window-data-len window))
      ((window-move window) window before)))

  (: window-print-line (window * fixnum -> string))
  (define (window-print-line window row nr-cols)
    (define (print-row window row row-cols)
      (let ((fmt (and (window-format window)
                      ((window-format window) (car row)))))
        (if fmt
          (scmus-format fmt row-cols (cdr row))
          (string-truncate (format "~a" (alist-ref 'text (cdr row) eqv? ""))
                           row-cols))))
    (if (>= (* 2 (window-h-border window)) nr-cols)
      (make-string nr-cols #\space)
      (let* ((h-border (window-h-border window))
             (str      (print-row window row (- nr-cols (* 2 h-border)))))
        (string-append (make-string h-border #\space)
                       str
                       (make-string h-border #\space)))))

  (: window-cursed (window * fixnum -> undefined))
  (define (window-cursed window row line-nr)
    ((*window-cursed window) window row line-nr))

  (: window-move-down! (window fixnum -> undefined))
  (define (window-move-down! window n)
    (let* ((top-pos  (window-top-pos window))
           (sel-pos  (window-sel-pos window))
           (data-len (window-data-len window))
           (nr-lines (window-nr-lines window))
           (can-move (max 0 (min n (- data-len sel-pos 1))))
           (scroll   (max 0 (+ 1 (- (+ sel-pos can-move)
                                    (+ top-pos nr-lines))))))
      (set! (window-top-pos window) (+ top-pos scroll))
      (set! (window-sel-pos window) (+ sel-pos can-move))
      (widget-damaged! window)))

  (: window-move-up! (window fixnum -> undefined))
  (define (window-move-up! window n)
    (let* ((top-pos (window-top-pos window))
           (sel-pos (window-sel-pos window))
           (can-move (min n sel-pos))
           (scroll (max 0 (- can-move
                             (- sel-pos top-pos)))))
      (set! (window-top-pos window) (- top-pos scroll))
      (set! (window-sel-pos window) (- sel-pos can-move))
      (widget-damaged! window)))

  (: window-move-top! (window -> undefined))
  (define (window-move-top! window)
    (window-select! window 0)
    (void))

  (: window-move-bottom! (window -> undefined))
  (define (window-move-bottom! window)
    (window-select! window (- (window-data-len window) 1))
    (void))

  ;; FIXME: this should be named window-move!, but it's taken
  (define (window-move-cursor! window n #!optional relative)
    (let ((nr-lines (if relative
                      (integer-scale (window-nr-lines window) n)
                      n)))
      (if (> nr-lines 0)
        (window-move-down! window nr-lines)
        (window-move-up! window (abs nr-lines))))
    (void))

  (: window-select! (window fixnum -> undefined))
  (define (window-select! window i)
    (let* ((sel-pos (window-sel-pos window))
           (diff (- sel-pos i)))
      (cond
        ((> diff 0) (window-move-up! window diff))
        ((< diff 0) (window-move-down! window (abs diff))))))

  (: window-search-init! (window string -> undefined))
  (define (window-search-init! window query)
    (set! (window-query window) query))

  (: *window-search (window string list -> (or fixnum boolean)))
  (define (*window-search window query data)
    (let loop ((data data) (i 0))
      (cond
        ((null? data) #f)
        (((window-match window) (car data) query) i)
        (else (loop (cdr data) (+ 1 i))))))

  (: window-next-match! (window -> (or fixnum boolean)))
  (define (window-next-match! window)
    (let* ((query    (window-query window))
           (data     (window-data window))
           (next-pos (+ 1 (window-sel-pos window)))
           (next-len (- (window-data-len window) next-pos))
           (shifted  (append (drop data next-pos)
                             (take data next-pos)))
           (match    (*window-search window query shifted)))
      (cond
        ((not match)         #f)
        ((>= match next-len) (- match next-len))
        (else                (+ next-pos match)))))

  (: window-prev-match! (window -> (or fixnum boolean)))
  (define (window-prev-match! window)
    (let* ((query    (window-query window))
           (data     (window-data window))
           (prev-pos (- (window-sel-pos window) 1))
           (shifted  (append (reverse (take data (+ 1 prev-pos)))
                             (reverse (drop data (+ 1 prev-pos)))))
           (match    (*window-search window query shifted)))
      (cond
        ((not match)        #f)
        ((> match prev-pos) (- (window-data-len window)
                               (abs (- prev-pos match))))
        (else               (- prev-pos match)))))

  (: window-search-next! (window -> undefined))
  (define (window-search-next! window)
    (let ((i (window-next-match! window)))
      (when i (window-select! window i))))

  (: window-search-prev! (window -> undefined))
  (define (window-search-prev! window)
    (let ((i (window-prev-match! window)))
      (when i (window-select! window i))))

  (: window-search! (window string -> undefined))
  (define (window-search! window query)
    (window-search-init! window query)
    (window-search-next! window))

(define-method (print-widget! (window <window>) x y cols rows)
  (let loop ((data (window-top window))
             (lines rows))
    (when (> lines 0)
      (let ((line-nr (+ y (- rows lines))))
        (if (null? data)
          (begin
            (cursed-set! CURSED-WIN)
            (print-line! "" x line-nr cols CURSED-WIN))
          (let ((cursed (window-cursed window (car data) line-nr)))
            (print-line! (window-print-line window (car data) cols)
                         x
                         line-nr
                         cols
                         cursed)))
        (loop (if (null? data) '() (cdr data)) (- lines 1))))))

  ;;
  ;; Window Stack
  ;;
  (define-class <window-stack> (<container>))

  (define (make-window-stack root-window #!rest windows)
    (let ((stack (make <window-stack> 'children (cons root-window windows))))
     (for-each (lambda (w) (set! (widget-parent w) stack))
               (cons root-window windows))
     stack))

  (define-method (compute-layout (stack <window-stack>) cols rows)
    (list (list (car (container-children stack)) 0 0 cols rows)))

  (define-method (widget-geometry-set! (stack <window-stack>) cols rows)
    (widget-geometry-set! (car (container-children stack)) cols rows))

  (define-method (window-stack-push! (stack <window-stack>) (window <window>))
    ; XXX: hack because window-geometry-set! is never called on new window
    (set! (window-nr-lines window) (window-nr-lines (car (container-children stack))))
    (container-prepend-child! stack window)
    (widget-damaged! stack))

  (define-method (window-stack-pop! (stack <window-stack>))
    (unless (null? (cdr (container-children stack)))
    (set! (container-children stack) (cdr (container-children stack))))
    (widget-damaged! stack))

  (define-method (window-stack-peek (stack <window-stack>))
    (let ((s (cdr (container-children stack))))
      (if (null? s)
        #f
        (car s))))

  ;; FIXME: not really the greatest place for this
  ;; Generates a function to call cursed-set! with the appropriate value given
  ;; a window, row, and line number.
  (: win-cursed-fn ((* -> boolean) -> (window * fixnum -> fixnum)))
  (define (win-cursed-fn current?)
    (lambda (window row line-nr)
      (let* ((current (current? row))
             (row-pos (+ (window-top-pos window) (- line-nr 1)))
             (selected (= row-pos (window-sel-pos window)))
             (marked (member row-pos (window-marked window))))
        (cond
          ((separator? row)       CURSED-WIN-TITLE)
          ((eqv? row 'separator)  CURSED-WIN-TITLE)
          ((and current selected) CURSED-WIN-CUR-SEL)
          (current                CURSED-WIN-CUR)
          (selected               CURSED-WIN-SEL)
          (marked                 CURSED-WIN-MARKED)
          (else                   CURSED-WIN))))))
