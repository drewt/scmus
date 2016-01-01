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

(declare (unit bindings-view)
  (uses editable event input keys ncurses ui-lib view window)
  (export make-bindings-view binding-edit!))

(define-type binding-row (struct binding-row))
(define-record-type binding-row
  (*make-binding-row context keys editable)
  binding-row?
  (context binding-row-context)
  (keys binding-row-keys)
  (editable binding-row-editable))

(: make-binding-row (symbol (list-of string) * -> binding-row))
(define (make-binding-row context keys expr)
  (*make-binding-row context keys
    (make-simple-editable binding-commit-edit!
                          (lambda (e) (set-input-mode! 'normal-mode))
                          binding-changed!
                          (with-output-to-string
                            (lambda () (write expr)))
                          (cons keys context))))

;; For sorting rows within a context.
(: binding-row<? (binding-row binding-row -> boolean))
(define (binding-row<? a b)
  (string<? (key-list->string (binding-row-keys a))
            (key-list->string (binding-row-keys b))))

;; For sorting contexts.  We want the common context to come first; the rest
;; should be in alphabetical order.
(: context<? ((pair symbol *) (pair symbol *) -> boolean))
(define (context<? a b)
  (let ((a-name (car a))
        (b-name (car b)))
    (cond
      ((eqv? a-name 'common) #t)
      ((eqv? b-name 'common) #f)
      (else (string<? (symbol->string a-name)
                      (symbol->string b-name))))))

(: binding-changed! thunk)
(define (binding-changed!)
  (register-event! 'binding-changed))

(: binding-edit! (window -> undefined))
(define (binding-edit! window)
  (let ((selected (window-selected window)))
    (when (binding-row? selected)
      (set-input-mode! 'edit-mode
                       (binding-row-editable selected)
                       (cons (+ 1 (window-sel-offset window))
                             (+ 1 (quotient (COLS) 2)))))))

(: binding-window-print-row (window * fixnum -> string))
(define (bindings-window-print-row window row nr-cols)
  (cond
    ((separator? row) (format "~a" (cdr row)))
    ((binding-row? row)
       (alist-print-line window
                         (cons (key-list->string (binding-row-keys row))
                               (editable-text (binding-row-editable row)))
                         nr-cols))))

(: binding-commit-edit! (editable -> boolean))
(define (binding-commit-edit! editable)
  (handle-exceptions e (begin (error-set! e) #f)
    (user-bind! (car (editable-data editable))
                (cdr (editable-data editable))
                (editable-read editable)
                #t)
    #t))

;; The bindings are stored as trees, where each node represents one key in a
;; key sequence.  We want to display this data as a list of key sequences, so
;; we use this function to flatten and append the trees.
(: make-bindings-data (-> (list-of binding-row)))
(define (make-bindings-data)
  (define (context->rows context)
    (define (binding-list->rows blist)
      (let loop ((blist blist) (keys '()))
        (if (binding-expression? blist)
          (list (make-binding-row (car context) keys (binding-data blist)))
          (apply append (map (lambda (x)
                               (loop (cdr x) (cons (car x) keys)))
                             blist)))))
    (if (null? (cdr context))
      '()
      (cons (cons 'separator (symbol->string (car context)))
            (sort! (binding-list->rows (cdr context)) binding-row<?))))
  (apply append (map context->rows (sort (bindings) context<?))))

(define-view bindings
  (make-view (make-window 'data       (make-bindings-data)
                          'changed    (lambda (w) (binding-changed!))
                          'activate   binding-edit!
                          'print-line bindings-window-print-row)
             " Key Bindings"
             edit:       binding-edit!))

(define-event-handler (binding-changed)
  (update-view! 'bindings))

(define-event-handler (binding-data-changed)
  (let ((window (get-window 'bindings)))
    (*window-data-set! window (make-bindings-data))
    (window-data-len-update! window)
    (update-view! 'bindings)))
