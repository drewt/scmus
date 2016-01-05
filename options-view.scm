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

(declare (unit options-view)
         (uses editable event format input ncurses option ui-lib view window)
         (export make-options-view option-edit!))

(: option-changed! thunk)
(define (option-changed!)
  (widget-damaged! (get-view 'options)))

(: option-edit! (window -> undefined))
(define (option-edit! window)
  (let* ((selected (window-selected window))
         (name (alist-ref 'name (cdr selected)))
         (editable (alist-ref 'text (cdr selected))))
    (set-input-mode! 'edit-mode
                     editable
                     (cons (+ 1 (window-sel-offset window))
                           (+ 1 (quotient (COLS) 2))))))

(define *option-format* (process-format "~-50%{name} ~{text}"))

(: options-window-print-row (window * fixnum -> string))
(define (options-window-print-row window row nr-cols)
  (scmus-format *option-format* nr-cols (cdr row)))

(: option-commit-edit! (editable -> boolean))
(define (option-commit-edit! editable)
  (handle-exceptions e (begin (error-set! e) #f)
    (set-option! (editable-data editable)
                 (editable-read editable))
    #t))

(: make-options-data (-> list))
(define (make-options-data)
  (define (option-editable pair)
    (make-simple-editable option-commit-edit!
                          (lambda (e) (set-input-mode! 'normal-mode))
                          option-changed!
                          (option-string (cdr pair))
                          (car pair)))
  (define (option->row pair)
    `(option . ((name . ,(car pair))
                (text . ,(option-editable pair)))))
  (map option->row (options)))

(define-view options
  (make-view (make-window 'data       (make-options-data)
                          'activate   option-edit!
                          'edit       option-edit!
                          'print-line options-window-print-row)
             " Options"))

(define-event-handler (option-data-changed) ()
  (let ((window (get-window 'options)))
    (set! (*window-data window) (make-options-data))))
