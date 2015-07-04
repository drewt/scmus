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

;;
;; A "binding list" is an alist associating keys with expressions or other
;; binding lists.  If a key is associated with an expression then the
;; expression is evaluated when the key is pressed.  If a key is associated
;; with a binding list, then scmus enters the context given by that binding
;; list when the key is pressed.
;; 
;; A "context" is a binding list.  When scmus is in the context of a particular
;; binding list, key-presses are interpreted relative to that list.  When a
;; key-press results in an expression being evaluated or a failure to locate a
;; binding, scmus returns to the top-level context from which it came.
;; 
;; The "top-level" contexts are enumerated in the *bindings* alist.  Before any
;; keys are pressed, scmus is in one of the top-level contexts.
;; 

(require-extension srfi-1)

(declare (unit keys)
         (uses eval-mode key-table ncurses)
         (export binding-context-valid? binding-data binding-expression
                 binding-expression? binding-key binding-keys-valid? bindings
                 enter-normal-mode key-list->string make-binding!
                 normal-mode-char normal-mode-key unbind!))

;; alist associating key-contexts with binding alists
(define *bindings*
  (map (lambda (x) (cons x '())) (cons 'common *view-names*)))

;; evil global state
(define *current-context* #f)
(define *common-context* #f)

(define (bindings) *bindings*)

(define (binding-expression? binding)
  (and (pair? binding) (eqv? (car binding) 'binding)))

(define (binding-key binding)
  (car binding))

(define (binding-data binding)
  (cdr binding))

(define (binding-expression binding)
  (cdr (binding-data binding)))

(define (get-binding key bindings)
  (assert (string? key) "get-binding" key)
  (assert (list? bindings) "get-binding" bindings)
  (alist-ref key bindings string=?))

;; Non-destructive binding update.  Binds expr to keys in key-list.
(define (make-binding keys key-list expr)
  (assert (and (list? keys) (not (null? keys)) (string? (car keys)))
          "make-binding" keys)
  (assert (list? key-list) "make-binding" key-list)
  (let ((binding (get-binding (car keys) key-list)))
    (cond
      ; last key and no conflict: do bind
      ((and (null? (cdr keys))
            (not binding))
        (alist-update (car keys) (cons 'binding expr) key-list string=?))
      ; binding conflict
      ((or (null? (cdr keys))
           (binding-expression? binding))
        #f)
      ; recursive case
      (else
        (let ((new-bindings (make-binding (cdr keys)
                                          (if binding binding '())
                                          expr)))
          (if new-bindings
            (alist-update (car keys) new-bindings key-list string=?)
            #f))))))

;; Destructive binding update.  Binds expr to keys in the given context.
(define (make-binding! keys context expr)
  (assert (and (list? keys) (not (null? keys)) (string? (car keys)))
          "make-binding!" keys)
  (assert (and (symbol? context) (binding-context-valid? context))
          "make-binding!" context)
  (let ((new (make-binding keys (alist-ref context *bindings*) expr)))
    (if new
      (begin
        (alist-update! context new *bindings*)
        (register-event! 'binding-data-changed)
        #t)
      #f)))

(define (keybind-remove key blist)
  (assert (string? key) "keybind-remove" key)
  (assert (list? blist) "keybind-remove" blist)
  (remove (lambda (x) (string=? (car x) key)) blist))

(define (unbind keys key-list)
  (assert (and (list? keys) (not (null? keys)) (string? (car keys)))
          "unbind" keys)
  (assert (list key-list) "unbind" key-list)
  (let ((binding (get-binding (car keys) key-list)))
    (cond
      ; base case: remove binding
      ((or (null? (cdr keys))
           (binding-expression? binding))
        (keybind-remove (car keys) key-list))
      ; not bound: do nothing
      ((not binding)
        key-list)
      ; recursive case
      (else
        (let ((new-bindings (unbind (cdr keys) binding)))
          (if (null? new-bindings)
            (keybind-remove (car keys) key-list)
            (alist-update (car keys) new-bindings key-list string=?)))))))

(define (unbind! keys context)
  (assert (and (list? keys) (not (null? keys)) (string? (car keys)))
          "unbind!" keys)
  (assert (and (symbol? context) (binding-context-valid? context))
          "unbind!" context)
  (let ((new (unbind keys (alist-ref context *bindings*))))
    (if new
      (begin
        (alist-update! context new *bindings*)
        (register-event! 'binding-data-changed)
        #t)
      #f)))

(define (key-list->string keys)
  (fold (lambda (str acc)
          (if acc
            (string-append acc " " str)
            str))
        #f
        keys))

;; Converts an ncurses keypress event to a string.
;; Argument may be either a character or an integer.
(define (key->string key)
  (assert (or (char? key) (integer? key)) "key->string" key)
  (find-key-name (if (char? key)
                   (char->integer key)
                   key)))

(define key-valid? find-key-code)

(define (binding-keys-valid? keys)
  (assert (list keys) "binding-keys-valid?" keys)
  (if (null? keys)
    #t
    (and (string? (car keys))
         (key-valid? (car keys))
         (binding-keys-valid? (cdr keys)))))

(define (binding-context-valid? context)
  (memv context (cons 'common *view-names*)))

;; Abandons the current key context.
(define (clear-context!)
  (set! *current-context* #f)
  (set! *common-context* #f))

;; Begins a new key context.  This can be delayed until a key is
;; pressed so that new bindings and view changes are taken into
;; account.
(define (start-context! view)
  (set! *current-context* (alist-ref view *bindings*))
  (set! *common-context* (alist-ref 'common *bindings*)))

(define (handle-user-key view key)
  (if (not *current-context*)
    (start-context! view))
  (let ((keystr (key->string key)))
    (if keystr
      (let ((view-binding (get-binding keystr *current-context*))
            (common-binding (get-binding keystr *common-context*))) 
        (cond
          ((binding-expression? view-binding)
            (user-eval (cdr view-binding))
            (clear-context!))
          ((binding-expression? common-binding)
            (user-eval (cdr common-binding))
            (clear-context!))
          ((or (list? view-binding)
               (list? common-binding))
            (set! *current-context* (if view-binding view-binding '()))
            (set! *common-context* (if common-binding common-binding '())))
          (else ; no binding
            (clear-context!)))))))

(define (enter-normal-mode)
  (cursor-off))

(define (normal-mode-char view ch)
  (case ch
    ((#\:) (enter-eval-mode))
    ((#\/) (enter-search-mode))
    ((#\q) (scmus-exit 0))
    (else (handle-user-key view ch))))

(define (normal-mode-key view key)
  (case key
    ((KEY_UP #f))
    ((KEY_DOWN #f))
    ((KEY_LEFT #f))
    ((KEY_RIGHT #f))
    (else (handle-user-key view key)))) 
