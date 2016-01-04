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

(require-extension coops)

(declare (unit view)
         (uses ui-lib)
         (hide *view-ctors*))

(: *views* (list-of (pair symbol (or boolean view))))
(define *views* (map (lambda (x) (cons x #f)) *view-names*))

(: *view-ctors* (list-of (pair symbol (-> view))))
(define *view-ctors* '())

(: register-view! (symbol (-> view) -> undefined))
(define (register-view! name ctor)
  (set! *view-ctors* (cons (cons name ctor) *view-ctors*)))

(: init-views! thunk)
(define (init-views!)
  (for-each (lambda (x)
              (alist-update! (car x) ((cdr x)) *views*))
            *view-ctors*))

(: get-view (symbol -> view))
(define (get-view name)
  (alist-ref name *views*))
