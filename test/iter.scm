;;
;; Copyright 2014-2020 Drew Thoreson
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

(include "test/test.scm")
(declare (unit test/iter)
         (uses iter))

(import drewt.iter)

(start-test "iter")

;; iter
(check (iter-empty? (iter)) => #t)
(check (iter-head?  (iter)) => #t)
(check (iter-node?  (iter)) => #f)
(check (iter->list (iter 1))     => '(1))
(check (iter->list (iter 1 2))   => '(1 2))
(check (iter->list (iter 1 2 3)) => '(1 2 3))

;; make-iter / iter-empty? / iter-length
(check (iter-empty? (make-iter 0)) => #t)
(check (iter-empty? (make-iter 1)) => #f)
(check (iter-length (make-iter 0)) => 0)
(check (iter-length (make-iter 1)) => 1)
(check (iter-head?  (make-iter 1)) => #t)

;; iter-head
(check (iter-head? (iter-head (iter)))                    => #t)
(check (iter-head? (iter-head (iter-prev (make-iter 3)))) => #t)
(check (iter-head? (iter-head (iter-next (make-iter 3)))) => #t)

;; iter-first / iter-last
(check (iter-head? (iter-prev (iter-first (iter-prev (make-iter 3))))) => #t)
(check (iter-head? (iter-next (iter-last  (iter-next (make-iter 3))))) => #t)

;; list->iter
(check (iter-empty? (list->iter '())) => #t)
(let ((iter (list->iter '(1 2 3))))
   (for-each (lambda (x) (check (iter-ref iter x) => x))
             '(1 2 3)))

;; iter->list
(check (iter->list (iter))       => '())
(check (iter->list (iter 1 2 3)) => '(1 2 3))

;; iter-ref - positive k
(let ((iter (iter 1 2 3)))
  (for-each (lambda (x) (check (iter-ref iter x) => x))
            '(1 2 3)))
;; list-ref - negative k
(let ((iter (iter -3 -2 -1)))
  (for-each (lambda (x) (check (iter-ref iter x) => x))
            '(-1 -2 -3)))
;; iter-walk - positive k
(let ((iter (iter 1 2 3)))
  (for-each (lambda (x) (check (iter-data (iter-walk iter x)) => x))
            '(1 2 3))
  (check (iter-head? (iter-walk iter 4)) => #t))
;; iter-walk - negative k
(let ((iter (iter -3 -2 -1)))
  (for-each (lambda (x) (check (iter-data (iter-walk iter x)) => x))
            '(-1 -2 -3))
  (check (iter-head? (iter-walk iter -4)) => #t))

;; iter-equal?
(check (iter-equal? (iter)       (iter))       => #t)
(check (iter-equal? (iter 1 2 3) (iter 1 2 3)) => #t)
(check (iter-equal? (iter 1 2)   (iter 1 2 3)) => #f)
(check (iter-equal? (iter 1 2 3) (iter 1 2))   => #f)
(check (iter-equal? (iter 1 2 3) (iter 3 2 1)) => #f)

;; iter-equal? - recursive
(check (iter-equal? (iter (iter 1 2 3)) (iter (iter 1 2 3))) => #t)
(check (iter-equal? (iter (iter 1 2 3)) (iter (iter 3 2 1))) => #f)

;; iter-add-before!
(check (iter-head (iter-add-before! (iter 1 2) 3)) (=> iter-equal?) (iter 1 2 3))
(check (iter-head (iter-add-before! (iter 1 (iter-cursor 3)) 2))
       (=> iter-equal?) (iter 1 2 3))

;; iter-add-after!
(check (iter-head (iter-add-after! (iter 2 3) 1)) (=> iter-equal?) (iter 1 2 3))
(check (iter-head (iter-add-after! (iter (iter-cursor 1) 3) 2))
       (=> iter-equal?) (iter 1 2 3))

;; iter-add-head!
(check (iter-head (iter-add-head! (iter) 1)) (=> iter-equal?) (iter 1))
(check (iter-head (iter-add-head! (iter 2 3) 1)) (=> iter-equal?) (iter 1 2 3))

;; iter-add-tail!
(check (iter-head (iter-add-tail! (iter) 1)) (=> iter-equal?) (iter 1))
(check (iter-head (iter-add-tail! (iter 1 2) 3)) (=> iter-equal?) (iter 1 2 3))

;; iter-remove!
(check (iter-head (iter-remove! (iter (iter-cursor 1) 2 3)))
       (=> iter-equal?) (iter 2 3))
(check (iter-head (iter-remove! (iter 1 (iter-cursor 2) 3)))
       (=> iter-equal?) (iter 1 3))
(check (iter-head (iter-remove! (iter 1 2 (iter-cursor 3))))
       (=> iter-equal?) (iter 1 2))

;; iter-fold
(check (iter-fold (lambda (x acc) #f) #t (iter)) => #t)
(check (iter-fold (lambda (x acc)
                    (+ acc x))
                  0
                  (iter 1 2 3))
       => 6)

;; iter-fold-nodes
(check (iter-fold-nodes (lambda (x acc) #f) #t (iter)) => #t)
(check (iter-fold-nodes (lambda (x acc)
                          (+ acc (iter-data x)))
                        0
                        (iter 1 2 3))
       => 6)

;; iter-fold-from
(check (iter-fold-from cons '() (iter)) => '())
(check (iter-fold-from cons '() (iter 1 2 3)) => '(3 2 1))
(check (iter-fold-from cons '() (iter (iter-cursor 1) 2 3)) => '(3 2 1))
(check (iter-fold-from cons '() (iter 1 (iter-cursor 2) 3)) => '(1 3 2))
(check (iter-fold-from cons '() (iter 1 2 (iter-cursor 3))) => '(2 1 3))

;; iter-for-each
(let ((acc 0))
  (define (add-to-acc x) (set! acc (+ acc x)))
  (check (begin (iter-for-each add-to-acc (iter))    acc) => 0)
  (check (begin (iter-for-each add-to-acc (iter 1 2 3)) acc) => 6))

;; iter-for-each-node
(let ((acc 0))
  (define (acc+ x) (set! acc (+ acc (iter-data x))))
  (check (begin (iter-for-each-node acc+ (iter)) acc) => 0)
  (check (begin (iter-for-each-node acc+ (iter 1 2 3)) acc) => 6))

;; iter-for-each-from
(let ((acc '()))
  (define (acc-cons x) (set! acc (cons x acc)))
  (check (begin (iter-for-each-from acc-cons (iter)) acc) => '())
  (check (begin (iter-for-each-from acc-cons (iter 1 2 3)) acc) => '(3 2 1)))

(define (plus1 x) (+ x 1))

;; iter-map
(check (iter-empty? (iter-map plus1 (iter))) => #t)
(check (iter-map plus1 (iter 1 2 3)) (=> iter-equal?) (iter 2 3 4))

;; iter-map!
(check (iter-empty? (iter-map plus1 (iter))) => #t)
(check (iter-map! plus1 (iter 1 2 3)) (=> iter-equal?) (iter 2 3 4))
(let ((tmp (iter 1 2 3)))
  (iter-map! plus1 tmp)
  (check tmp (=> iter-equal?) (iter 2 3 4)))

;; iter-map-from
(check (iter-empty? (iter-map-from plus1 (iter))) => #t)
(check (iter-map-from plus1 (iter 1 (iter-cursor 2) 3))
       (=> iter-equal?) (iter 3 4 2))

;; iter-map->list
(check (iter-map->list plus1 (iter)) => '())
(check (iter-map->list plus1 (iter 1 2 3)) => '(2 3 4))

;; iter-filter
(check (iter-empty? (iter-filter odd? (iter))) => #t)
(check (iter-filter odd? (iter 1 2 3)) (=> iter-equal?) (iter 1 3))

;; iter-filter!
(check (iter-empty? (iter-filter! odd? (iter))) => #t)
(check (iter-filter! odd? (iter 1 2 3)) (=> iter-equal?) (iter 1 3))
(let ((tmp (iter 1 2 3)))
  (iter-filter! odd? tmp)
  (check tmp (=> iter-equal?) (iter 1 3)))

(end-test "iter")
