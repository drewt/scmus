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

(require-extension coops)

(declare (hide *view-ctors*))

(import scmus.base)
(import scmus.tui)

(: *views* (list-of (pair symbol (or boolean frame))))
(define *views* (map (lambda (x) (cons x #f)) *view-names*))

(: *view-ctors* (list-of (pair symbol (-> frame))))
(define *view-ctors* '())

(: register-view! (symbol (-> frame) -> undefined))
(define (register-view! name ctor)
  (set! *view-ctors* (cons (cons name ctor) *view-ctors*)))

(: init-views! thunk)
(define (init-views!)
  (for-each (lambda (x)
              (alist-update! (car x) ((cdr x)) *views*))
            *view-ctors*))

(: get-view (symbol -> frame))
(define (get-view name)
  (alist-ref name *views*))

(: view-add! (frame -> undefined))
(define (view-add! view)
  (window-add! (view-window view)))

(: view-remove! (frame -> undefined))
(define (view-remove! view)
  (window-remove! (view-window view)))

(: view-clear! (frame -> undefined))
(define (view-clear! view)
  (window-clear! (view-window view)))

(: view-edit! (frame -> undefined))
(define (view-edit! view)
  (window-edit! (view-window view)))

(: view-move! (frame boolean -> undefined))
(define (view-move! view before)
  (window-move! (view-window view) before))

(define (view-window view)
  (widget-focus view))
