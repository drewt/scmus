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

(import ncurses)

(define-record-type key-list (make-key-list keys) key-list?
  (keys key-list-keys))

(define-record-printer (key-list kl out)
  (display (key-list->string (key-list-keys kl)) out))

(define-type binding-row (pair symbol (list-of (pair symbol *))))

(: make-binding-row (symbol (list-of string) * -> binding-row))
(define (make-binding-row context keys expr)
  `(binding . ((context . ,context)
               (key     . ,(make-key-list keys))
               (value   . ,(make-simple-editable
                             binding-commit-edit!
                             (lambda (e) (set-input-mode! 'normal-mode))
                             binding-changed!
                             (with-output-to-string
                               (lambda () (write expr)))
                             (cons keys context))))))

(define (binding-row? row)
  (and (pair? row) (eqv? (car row) 'binding)))

(define (binding-row-context row)
  (alist-ref 'context (cdr row)))

(define (binding-row-keys row)
  (key-list-keys (alist-ref 'key (cdr row))))

(define (binding-row-editable row)
  (alist-ref 'value (cdr row)))

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
  (widget-damaged! (get-view 'bindings)))

(: binding-edit! (window -> undefined))
(define (binding-edit! window)
  (let ((selected (window-selected window)))
    (when (binding-row? selected)
      (set-input-mode! 'edit-mode
                       (binding-row-editable selected)
                       (cons (+ 1 (window-sel-offset window))
                             (+ 1 (quotient (COLS) 2)))))))

(: binding-format (symbol -> format-spec))
(define (binding-format tag)
  (case tag
    ((separator) (get-format 'format-separator))
    ((binding)   *key-value-format*)))

(: binding-commit-edit! (editable -> boolean))
(define (binding-commit-edit! editable)
  (handle-exceptions e (begin (error-set! e) #f)
    (user-bind! (car (editable-data editable))
                (cdr (editable-data editable))
                (editable-read editable)
                #t)
    #t))

(define (make-separator text)
  `(separator . ((text . ,text))))

;; The bindings are stored as trees, where each node represents one key in a
;; key sequence.  We want to display this data as a list of key sequences, so
;; we use this function to flatten and append the trees.
(: make-bindings-data (-> (list-of binding-row)))
(define (make-bindings-data)
  (define (context->rows context)
    (define (binding-list->rows blist)
      (let loop ((blist blist) (keys '()))
        (if (binding-expression? blist)
          (list (make-binding-row (car context) (reverse keys) (binding-data blist)))
          (apply append (map (lambda (x)
                               (loop (cdr x) (cons (car x) keys)))
                             blist)))))
    (if (null? (cdr context))
      '()
      (cons (make-separator (string-titlecase (symbol->string (car context))))
            (sort! (binding-list->rows (cdr context)) binding-row<?))))
  (apply append (map context->rows (sort (bindings) context<?))))

(define-view bindings
  (make-view (make-window 'data       (make-bindings-data)
                          'activate   binding-edit!
                          'edit       binding-edit!
                          'format     binding-format)
             " Key Bindings"))

(define-event-handler (binding-data-changed) ()
  (let ((window (get-window 'bindings)))
    (set! (*window-data window) (make-bindings-data))))
