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
         (uses editable event input ncurses option ui-lib view window)
         (export make-options-view option-edit!))

(: option-changed! thunk)
(define (option-changed!)
  (register-event! 'option-changed))

(: option-edit! (window -> undefined))
(define (option-edit! window)
  (let* ((selected (window-selected window))
         (name (car selected))
         (editable (cdr selected)))
    (set-input-mode! 'edit-mode
                     editable
                     (cons (+ 1 (window-sel-offset window))
                           (quotient (COLS) 2)))))

(: options-window-print-row (window * fixnum fixnum -> undefined))
(define (options-window-print-row window row line-nr cursed)
  (alist-print-line window
                    (cons (car row) (editable-text (cdr row)))
                    line-nr
                    cursed))

(: option-commit-edit! (editable -> boolean))
(define (option-commit-edit! editable)
  (handle-exceptions e (begin (error-set! e) #f)
    (set-option! (editable-data editable)
                 (editable-read editable))
    #t))

(: make-options-data (-> list))
(define (make-options-data)
  (define (option->row pair)
    (cons (car pair)
          (make-simple-editable option-commit-edit!
                                (lambda (e) (set-input-mode! 'normal-mode))
                                option-changed!
                                (option-string (cdr pair))
                                (car pair))))
  (map option->row (options)))

(define-view options
  (make-view (make-window data:     (make-options-data)
                          changed:  (lambda (w) (option-changed!))
                          activate: option-edit!)
             "Options"
             print-line: options-window-print-row
             edit:       option-edit!))

(define-event-handler (option-changed)
  (update-view! 'options))

(define-event-handler (option-data-changed)
  (let ((window (get-window 'options)))
    (*window-data-set! window (make-options-data))
    (window-data-len-update! window)
    (update-view! 'options)))
