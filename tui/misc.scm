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

  (define-class <text> (<widget>)
    ((text initform: ""
           reader:   text-text)))

  (define (make-text str . kwargs)
    (apply make <text> 'text str kwargs))

  (define-method ((setter text-text) (text <text>) str)
    (set! (slot-value text 'text) str)
    (widget-damaged! text))

  (define-method (print-widget! (text <text>) x y cols rows)
    (for-each (lambda (line) (print-line! line x y cols))
      (take-at-most (string-split-lines (text-text text)) rows)))

  ; FIXME: is there any value in subclassing <text> here?
  (define-class <format-text> (<text>)
    ((format initform: (process-format "")
             accessor: format-text-format)
     (data   initform: '()
             reader:   format-text-data)))

  (define (make-format-text format data . kwargs)
    (apply make <format-text> 'text format
                              'format (process-format format)
                              'data data kwargs))

  (define-method ((setter format-text-format) (text <format-text>) format)
    (set! (slot-value text 'text) format)
    (set! (slot-value text 'format) (process-format format))
    (widget-damaged! text))

  (define-method ((setter format-text-data) (text <format-text>) data)
    (set! (slot-value text 'data) data)
    (widget-damaged! text))

  (define-method (print-widget! (text <format-text>) x y cols rows)
    (for-each (lambda (line) (print-line! line x y cols))
      (take-at-most (string-split-lines (scmus-format (format-text-format text)
                                                      cols
                                                      (format-text-data text)))
                    rows))))
