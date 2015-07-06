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

(declare (unit view)
         (uses ui-lib)
         (hide *view-ctors*))

(: *make-view (window
               format-spec
               (window * fixnum fixnum -> undefined)
               (window * fixnum -> fixnum)
               (window -> undefined)
               (window -> undefined)
               (window -> undefined)
               (window -> undefined)
               (window -> undefined)
                 -> view))
(: view-window (view -> window))
(: view-window-set! (view window -> undefined))
(: view-title-fmt (view -> format-spec))
(: view-title-fmt-set! (view format-spec -> undefined))
(: *view-print-line (view -> (window * fixnum fixnum -> undefined)))
(: view-cursed-fn (view -> (window * fixnum -> fixnum)))
(: view-add (view -> (window -> undefined)))
(: view-remove (view -> (window -> undefined)))
(: view-clear (view -> (window -> undefined)))
(: view-edit (view -> (window -> undefined)))
(: view-move (view -> (window -> undefined)))
(define-record-type view
  (*make-view window title-fmt print-line cursed add! remove! clear! edit!
              move!)
  view?
  (window view-window view-window-set!)
  (title-fmt view-title-fmt view-title-fmt-set!)
  (print-line *view-print-line)
  (cursed view-cursed-fn)
  (add! view-add)
  (remove! view-remove)
  (clear! view-clear)
  (edit! view-edit)
  (move! view-move))

(define (make-view window title print-line
                   #!key
                   (cursed generic-cursed-set!)
                   (add void)
                   (remove void)
                   (clear void)
                   (edit void)
                   (move void))
  (*make-view window (process-format title) print-line cursed add remove clear
              edit move))

(: view-print-line! (view * fixnum fixnum -> undefined))
(define (view-print-line! view row line-nr cursed)
  ((*view-print-line view) (view-window view) row line-nr cursed))

(: view-cursed-set! (view * fixnum -> fixnum))
(define (view-cursed-set! view row line-nr)
  ((view-cursed-fn view) (view-window view) row line-nr))

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
