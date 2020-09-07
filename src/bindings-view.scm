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

(declare (hide))

(import (drewt ncurses)
        (scmus base)
        (scmus error)
        (scmus event)
        (scmus keys)
        (scmus tui)
        (scmus view)
        (scmus widgets))

(define-record-type key-list (make-key-list keys) key-list?
  (keys key-list-keys))

(define-record-printer (key-list kl out)
  (display (string-append " " (key-list->string (key-list-keys kl))) out))

(define-class <binding-row> (<split-pane>)
  ((context accessor: binding-row-context)
   (keys    accessor: binding-row-keys)
   (expr    accessor: binding-row-expr)))

(define (make-binding-row context keys expr . args)
  (let* ((keys (make-key-list keys))
         (text (if (and (pair? expr)
                        (= (length expr) 2)
                        (eqv? (car expr) '*command)
                        (string? (cadr expr)))
                 (cadr expr)
                 (format #f "~s" expr)))
         (input (make-text-input text "")))
    (add-listener input 'commit binding-commit-edit!)
    (apply make <binding-row>
                'context context
                'keys keys
                'expr expr
                'left-child (make-scheme-text keys)
                'right-child input
                args)))

(define (binding-commit-edit! str)
  (let* ((widget  (current-event-source))
         (text    (string-trim-both str))
         (context (binding-row-context (widget-parent widget)))
         (keys    (key-list-keys (binding-row-keys (widget-parent widget)))))
    (handle-exceptions e (begin (scmus-error e) #f)
      (unbind! keys context)
      (when (> (string-length text) 0)
        (if (char=? (string-ref text 0) #\()
          (make-binding! keys context (with-input-from-string text read))
          (make-binding! keys context `(*command ,text)))))))

(define (make-bindings-data)
  (define (context<? a b)
    (let ((a-name (car a))
          (b-name (car b)))
      (cond
        ((eqv? a-name 'common) #t)
        ((eqv? b-name 'common) #f)
        (else (string<? (symbol->string a-name)
                        (symbol->string b-name))))))
  (define (binding-row<? a b)
    (string<? (key-list->string (key-list-keys (binding-row-keys a)))
              (key-list->string (key-list-keys (binding-row-keys b)))))
  (define (binding-list->rows blist context)
    (let loop ((blist blist) (keys '()))
      (if (binding-expression? blist)
        (list (make-binding-row context (reverse keys) (binding-data blist)))
        (apply append (map (lambda (x)
                             (loop (cdr x) (cons (car x) keys)))
                           blist)))))
  (define (context->rows context)
    (if (null? (cdr context))
      '()
      (cons (make <window-separator> 'text (string-titlecase
                                             (string-append " " (symbol->string (car context))))
                                     'cursed CURSED-WIN-TITLE)
            (sort! (binding-list->rows (cdr context) (car context)) binding-row<?))))
  (apply append (map context->rows (sort (bindings) context<?))))

(define-class <bindings-window> (<window>))

(define-method (widget-edit (window <bindings-window>))
  (unless (list-box-empty? window)
    (text-input-begin (split-pane-right-child (list-box-selected window)) steal-focus: #t)))

(define-method (widget-activate (window <bindings-window>))
  (widget-edit window))

(define *bindings-window*
  (make <bindings-window>
        'data       (make-bindings-data)
        'cursed     CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define-view bindings
  (make-frame 'body   *bindings-window*
              'header (make-text " Key Bindings" 'cursed CURSED-WIN-TITLE)))

(add-listener/global 'binding-data-changed
  (lambda ()
    (set! (list-box-data *bindings-window*) (make-bindings-data))))
