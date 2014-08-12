;;
;; Copyright 2014 Drew Thoreson
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

(require-extension ncurses srfi-1 srfi-13)

(declare (unit options-view)
         (uses option ui-curses window)
         (export make-options-view))

(define (option-activate! window)
  (let* ((selected (window-selected window))
         (name (car selected))
         (option (cdr selected)))
    (push! (format "(set-option! '~a ~a)" name (option-string option)))))

(define (options-window-print-row window row line-nr)
  (alist-print-line window
                   (cons (car row) (option-string (cdr row)))
                   line-nr))

(define (make-options-view)
  (make-view (make-window #f
                          (lambda (w) *options*)
                          (lambda (w) (register-event! 'option-changed))
                          option-activate!
                          void
                          (lambda (e q) #f))
             "Options"
             options-window-print-row))
