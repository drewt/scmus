;;
;; Copyright 2014-2017 Drew Thoreson
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
         (uses eval-mode event key-table ncurses)
         (export binding-context-valid? binding-data binding-expression
                 binding-expression? binding-key binding-keys-valid? bindings
                 key-list->string make-binding! normal-mode-char
                 normal-mode-key unbind!))

(import scmus-base event ncurses)

(define-type binding-node (pair string pair))
(define-type binding-list (list-of binding-node))
(define-type binding-terminal (pair symbol *))
(define-type binding-data (or binding-list binding-terminal))

;; alist associating key-contexts with binding alists
(: *bindings* (list-of (pair symbol binding-list)))
(define *bindings*
  (map (lambda (x) (cons x '())) (cons 'common *view-names*)))

;; evil global state
(: *current-context* (or boolean binding-list))
(define *current-context* #f)

(: *common-context* (or boolean binding-list))
(define *common-context* #f)

(: bindings (-> (list-of (pair symbol binding-list))))
(define (bindings) *bindings*)

(: binding-expression? (* -> boolean))
(define (binding-expression? binding)
  (and (pair? binding) (eqv? (car binding) 'binding)))

(: binding-key (pair -> string))
(define (binding-key binding)
  (car binding))

(: binding-data (pair -> binding-data))
(define (binding-data binding)
  (cdr binding))

(: binding-expression (pair -> *))
(define (binding-expression binding)
  (cdr (binding-data binding)))

(: get-binding (string binding-list -> binding-data))
(define (get-binding key bindings)
  (alist-ref key bindings string=?))

;; Non-destructive binding update.  Binds expr to keys in key-list.
(: make-binding ((list-of string) binding-list * -> binding-list))
(define (make-binding keys key-list expr)
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
(: make-binding! ((list-of string) symbol * -> boolean))
(define (make-binding! keys context expr)
  (assert (binding-context-valid? context) "make-binding!" context)
  (let ((new (make-binding keys (alist-ref context *bindings*) expr)))
    (if new
      (begin
        (alist-update! context new *bindings*)
        (register-event! 'binding-data-changed)
        #t)
      #f)))

(: keybind-remove (string binding-list -> binding-list))
(define (keybind-remove key blist)
  (remove (lambda (x) (string=? (car x) key)) blist))

(: unbind ((list-of string) binding-list -> binding-list))
(define (unbind keys key-list)
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

(: unbind! ((list-of string) symbol -> boolean))
(define (unbind! keys context)
  (assert (binding-context-valid? context) "unbind!" context)
  (let ((new (unbind keys (alist-ref context *bindings*))))
    (if new
      (begin
        (alist-update! context new *bindings*)
        (register-event! 'binding-data-changed)
        #t)
      #f)))

(: key-list->string ((list-of string) -> string))
(define (key-list->string keys)
  (fold (lambda (str acc)
          (if acc
            (string-append acc " " str)
            str))
        #f
        keys))

;; Converts an ncurses keypress event to a string.
;; Argument may be either a character or an integer.
(: key->string ((or char fixnum) -> string))
(define (key->string key)
  (find-key-name (if (char? key)
                   (char->integer key)
                   key)))

(: key-valid? (* -> boolean))
(define key-valid? find-key-code)

(: binding-keys-valid? (list -> boolean))
(define (binding-keys-valid? keys)
  (if (null? keys)
    #t
    (and (string? (car keys))
         (key-valid? (car keys))
         (binding-keys-valid? (cdr keys)))))

(: binding-context-valid? (symbol -> boolean))
(define (binding-context-valid? context)
  (memv context (cons 'common *view-names*)))

;; Abandons the current key context.
(: clear-context! thunk)
(define (clear-context!)
  (set! *current-context* #f)
  (set! *common-context* #f))

;; Begins a new key context.  This can be delayed until a key is
;; pressed so that new bindings and view changes are taken into
;; account.
(: start-context! (symbol -> undefined))
(define (start-context! view)
  (set! *current-context* (alist-ref view *bindings*))
  (set! *common-context* (alist-ref 'common *bindings*)))

(: handle-user-key (symbol fixnum -> undefined))
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

(: normal-mode-char (symbol char -> undefined))
(define (normal-mode-char view ch)
  (case ch
    ((#\:) (enter-eval-mode))
    ((#\/) (enter-search-mode))
    ((#\q) (scmus-exit 0))
    (else (handle-user-key view ch))))

(: normal-mode-key (symbol fixnum -> undefined))
(define (normal-mode-key view key)
  (case key
    ((KEY_UP #f))
    ((KEY_DOWN #f))
    ((KEY_LEFT #f))
    ((KEY_RIGHT #f))
    (else (handle-user-key view key)))) 
