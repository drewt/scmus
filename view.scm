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
(: view-move (view -> (window boolean -> undefined)))
(define-record/initform view *make-view view?
  (window #f view-window view-window-set!)
  (title-fmt #f view-title-fmt view-title-fmt-set!)
  (print-line list-window-print-row *view-print-line)
  (cursed generic-cursed-set! view-cursed-fn)
  (add void view-add)
  (remove void view-remove)
  (clear void view-clear)
  (edit void view-edit)
  (move void view-move))

(define (make-view window title . kwargs)
  (apply *make-view window:    window
                    title-fmt: (process-format title)
                    kwargs))

(: view-print-line! (view * fixnum fixnum -> undefined))
(define (view-print-line! view row line-nr cursed)
  ((*view-print-line view) (view-window view) row line-nr cursed))

(: view-cursed-set! (view * fixnum -> fixnum))
(define (view-cursed-set! view row line-nr)
  ((view-cursed-fn view) (view-window view) row line-nr))

(: view-add! (view -> undefined))
(define (view-add! view)
  ((view-add view) (view-window view)))

(: view-remove! (view -> undefined))
(define (view-remove! view)
  ((view-remove view) (view-window view)))

(: view-clear! (view -> undefined))
(define (view-clear! view)
  ((view-clear view) (view-window view)))

(: view-edit! (view -> undefined))
(define (view-edit! view)
  ((view-edit view) (view-window view)))

(: view-move! (view boolean -> undefined))
(define (view-move! view before)
  ((view-move view) (view-window view) before))

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
