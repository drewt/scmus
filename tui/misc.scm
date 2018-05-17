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

(module scmus.tui.misc *
  (import coops
          drewt.ncurses
          scmus.base
          scmus.format
          scmus.tui.display
          scmus.tui.widget)

  (define-class <separator> (<widget>)
    ((char initform: #\space
           accessor: separator-char)))

  (define-method (print-widget! (separator <separator>) x y cols rows)
    (let loop ((row y))
      (when (< (- row y) rows)
        (move row x)
        (let loop ((col 0))
          (when (< col cols)
            (addch (separator-char separator))
            (loop (+ col 1))))
        (loop (+ row 1)))))

  (define-class <pile> (<container>)
    ((children initform: '()
               accessor: container-children)))

  (define (make-pile children . kwargs)
    (apply make <pile> 'children children kwargs))

  (define-method (widget-size (pile <pile>) cols rows)
    (values cols
      (let loop ((children (container-children pile)) (count 0))
        (cond
          ((null? children) count)
          ((>= count rows)  rows)
          (else
            (let-values (((_ child-rows) (widget-size (car children) cols (- rows count))))
              (loop (cdr children) (+ count child-rows))))))))

  (define-method (compute-layout (pile <pile>) cols rows)
    (let loop ((rows rows)
               (y 0)
               (children (container-children pile))
               (result '()))
      (if (or (<= rows 0) (null? children))
        (reverse result)
        (let-values (((_ child-rows) (widget-size (car children) cols rows)))
          (loop (- rows child-rows)
                (+ y child-rows)
                (cdr children)
                (cons (list (car children) 0 y cols child-rows)
                      result))))))

  (define-class <textual> (<widget>))

  ;; Method to retrieve/generate the text data.  Should return a list of
  ;; strings, where each item in the list is a line (which SHOULD NOT
  ;; contain the #\newline character).
  (define-abstract-method (text-text (text <textual>)))

  (define-method (print-widget! (text <textual>) x y cols rows)
    (for-each (lambda (line) (print-line! line x y cols))
      (take-at-most (text-text text) rows)))

  (define-class <text> (<textual>)
    ((text initform: '("")
           reader:   text-text)
     (w    initform: 0
           reader:   text-w)
     (h    initform: 0
           reader:   text-h)))

  (define (make-text str . kwargs)
    (let ((text (apply make <text> kwargs)))
      (set! (text-text text) str)
      text))

  (define-method ((setter text-text) (text <text>) str)
    (let ((lines (string-split-lines str)))
      (set! (slot-value text 'text) lines)
      (set! (slot-value text 'w)    (fold max 0 (map string-length lines)))
      (set! (slot-value text 'h)    (length lines)))
    (widget-damaged! text))

  (define-method (widget-size (text <text>) available-cols available-rows)
    (values (min available-cols (text-w text))
            (min available-rows (text-h text))))

  (define-class <format-text> (<textual>)
    ((format initform: (process-format "")
             accessor: format-text-format)
     (data   initform: '()
             accessor: format-text-data)))

  (define (make-format-text format data . kwargs)
    (apply make <format-text> 'format (if (string? format)
                                        (process-format format)
                                        format)
                              'data data kwargs))

  (define-method ((setter format-text-format) after: (text <format-text>) fmt)
    (widget-damaged! text))

  (define-method ((setter format-text-data) after: (text <format-text>) data)
    (widget-damaged! text))

  (define-method (widget-size (text <format-text>) available-cols available-rows)
    (values available-cols (min available-rows 1)))

  (define-method (text-text (text <format-text>))
    (list (scmus-format (format-text-format text) (widget-cols text) (format-text-data text)))))
