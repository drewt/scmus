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
         (uses editable option ui-curses window)
         (export make-options-view option-edit! update-options-data))

(define (option-changed!)
  (register-event! 'option-changed))

(define (editable-read editable)
  (condition-case (with-input-from-string (editable-text editable) read)
    (e () (error-set! e))))

(define (option-char editable ch)
  (case ch
    ((#\newline)
      (set-option! (car (window-selected (view-window 'options)))
                   (editable-read editable))
      (set-input-mode! 'normal-mode))
    (else (editable-default-char-handler editable ch)))
  (option-changed!))

(define (option-key editable key)
  (editable-default-key-handler editable key)
  (option-changed!))

(define (option-init editable)
  (cursor-on)
  (editable-default-init editable))

(define (option-edit! window)
  (let* ((selected (window-selected window))
         (name (car selected))
         (editable (cdr selected)))
    (set-input-mode! 'edit-mode
                     editable
                     (cons (+ 1 (window-sel-pos window))
                           (quotient (COLS) 2)))))

(define (options-window-print-row window row line-nr)
  (alist-print-line window
                    (cons (car row) (editable-text (cdr row)))
                    line-nr))

(define (make-options-data)
  (define (option->row pair)
    (cons (car pair)
          (make-editable-with-text (option-string (cdr pair))
                                   option-char
                                   option-key
                                   option-init)))
  (map option->row *options*))

(define (make-options-view)
  (make-view (make-window (make-options-data)
                          *window-data
                          (lambda (w) (register-event! 'option-changed))
                          option-edit!
                          void
                          (lambda (e q) #f))
             "Options"
             options-window-print-row))

(define (update-options-data window)
  (*window-data-set! window (make-options-data))
  (window-data-len-update! window)
  (update-view! 'options))
