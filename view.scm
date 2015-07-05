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

(define (view-print-line! view row line-nr cursed)
  ((*view-print-line view) (view-window view) row line-nr cursed))

(define (view-cursed-set! view row line-nr)
  ((view-cursed-fn view) (view-window view) row line-nr))

(define *views* (map (lambda (x) (cons x #f)) *view-names*))

(define *view-ctors* '())

(define (register-view! name ctor)
  (set! *view-ctors* (cons (cons name ctor) *view-ctors*)))

(define (init-views!)
  (for-each (lambda (x)
              (alist-update! (car x) ((cdr x)) *views*))
            *view-ctors*))