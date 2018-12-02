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

(module scmus.tui.input *
  (import coops
          drewt.ncurses
          scmus.base
          scmus.tui.widget
          scmus.tui.misc)

  ;; Text widget of the form '<prefix><text>' where
  ;;   <prefix> is optional descriptive text
  ;;   <text>   is editable text
  ;; TODO: scroll when the cursor moves beyond the edge of the available area
  (define-class <text-input> (<textual>)
    ((text       initform: '()
                 accessor: text-input-text)
     (saved-text initform: '()
                 accessor: text-input-saved-text)
     (cursor-pos initform: 0
                 accessor: text-input-cursor-pos)
     ; TODO: allow specifying percentage of available cols to allocate to prefix
     (prefix     initform: ""
                 accessor: text-input-prefix)
     (editing?   initform: #f
                 accessor: text-input-editing?)
     (on-commit  initform: (lambda (_) #f)
                 accessor: text-input-on-commit)
     (on-cancel  initform: (lambda (_) #f)
                 accessor: text-input-on-cancel)
     (on-begin   initform: (lambda (_) #f)
                 accessor: text-input-on-begin)
     (on-leave   initform: (lambda (_) #f)
                 accessor: text-input-on-leave)))

  (define (make-text-input text prefix . args)
    (apply make <text-input>
                'text (reverse (string->list text))
                'prefix prefix
                args))

  (define-method (text-text (text <text-input>))
    (list (string-append (text-input-prefix text)
                         (list->string (reverse (text-input-text text))))))

  (define-method (widget-size (text <text-input>) available-cols available-rows)
    (values available-cols (min available-rows 1)))

  (define (text-input-update-cursor input)
    (move (widget-y input)
          (min (- (COLS) 1)
               (+ (widget-x input)
                  (string-length (text-input-prefix input))
                  (- (text-input-length input)
                     (text-input-cursor-pos input))))))

  (define-method ((setter text-input-editing?) after: (widget <text-input>) editing?)
    (if editing?
      (begin (text-input-update-cursor widget)
             (curs_set 1))
      (curs_set 0)))

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

  (define-method (handle-input (widget <text-input>) input)
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
      (if (eqv? input #\newline)
        (text-input-begin widget)
        (call-next-method)))
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
    ((text-input-on-leave widget) widget))

  (define-method (text-input-cancel (widget <text-input>))
    (set! (text-input-editing? widget) #f)
    (set! (text-input-text widget)
          (text-input-saved-text widget))
    (set! (text-input-saved-text widget) '())
    ((text-input-on-cancel widget) widget)
    ((text-input-on-leave widget) widget))

  (define-method (text-input-begin (widget <text-input>))
    (set! (text-input-editing? widget) #t)
    (set! (text-input-saved-text widget)
          (text-input-text widget))
    ((text-input-on-begin widget) widget))

  (define (do-handle-input widget input)
    (handle-input (widget-focus widget) input)))
