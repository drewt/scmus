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

(include "test/test.scm")
(declare (unit test/trie)
         (uses trie))

(import drewt.trie)

(start-test "trie")

;; trie?
(check (trie? (empty-trie)) => #t)
(check (trie? '(1 2 3))     => #f)

(define (key<? a b)
  (cond
    ((and (null? a) (null? b)) #f)
    ((null? a) #t)
    ((null? b) #f)
    ((< (car a) (car b)) #t)
    ((= (car a) (car b)) (key<? (cdr a) (cdr b)))
    (else #f)))

(define (alist-sort a)
  (sort a (lambda (a b) (key<? (car a) (car b)))))

;; trie->alist / alist->trie
(define (check-alist a)
  (check (alist-sort (trie->alist (alist->trie a))) => (alist-sort a)))

(check-alist '())
(check-alist '(((1)   . one)))
(check-alist '(((1)   . one)
               ((2)   . two)))
(check-alist '(((1)   . one)
               ((1 2) . one-two)))
(check-alist '(((1)   . one)
               ((1 2) . one-two)
               ((1 3) . one-three)))

;; trie->alist/prefix
(define (check-alist/prefix prefix a)
  (define (alist/prefix a prefix)
    (let ((len (length prefix)))
      (filter (lambda (k/v)
                (and (>= (length (car k/v)) len)
                     (equal? (take (car k/v) len) prefix)))
              a)))
  (check (alist-sort (trie->alist/prefix (alist->trie a) prefix))
      => (alist-sort (alist/prefix a prefix))))

(check-alist/prefix '()  '())
(check-alist/prefix '(1) '())
(check-alist/prefix '()  '(((1)   . one)))
(check-alist/prefix '(1) '(((1 2) . one-two)))
(check-alist/prefix '(1) '(((1)   . one)
                           ((1 2) . one-two)))
(check-alist/prefix '(1) '(((1 1) . one-one)
                           ((1 2) . one-two)))

;; trie-keys
(define (check-keys a)
  (let ((trie (alist->trie a)))
    (check (sort (trie-keys trie) key<?)
        => (sort (map car a) key<?))))

(check-keys '())
(check-keys '(((1)   . one)))
(check-keys '(((1 2) . one-two)))
(check-keys '(((1)   . one)
              ((1 2) . one-two)))
(check-keys '(((1)   . one)
              ((2)   . two)))
(check-keys '(((1 1) . one-one)
              ((1 2) . one-two)))

;; trie-ref
(check (trie-ref (empty-trie) '()) => #f)
(check (trie-ref (empty-trie) '(1)) => #f)
(check (trie-ref (empty-trie) '(1) #t) => #t)
(let* ((alist '(((1)   . one)
                ((1 1) . one-one)
                ((1 2) . one-two)
                ((2 1) . two-one)
                ((3)   . three)))
       (trie (alist->trie alist)))
  (for-each (lambda (k/v)
              (check (trie-ref trie (car k/v)) => (cdr k/v)))
            alist)
  (check (trie-ref trie '(2)) => #f)
  (check (trie-ref trie '(1 3)) => #f))

;; trie-ref/prefix
(define (check-prefix-ref trie prefix value key)
  (check (nth-value 0 (trie-ref/prefix trie prefix)) => value)
  (check (nth-value 1 (trie-ref/prefix trie prefix)) => key))

(let ((trie (alist->trie '(((1 2 3 4) . one-two-three-four)
                           ((1 3 4 5) . one-three-four-five)
                           ((1 3 5 6) . one-three-five-six)
                           ((2 3)     . two-three)))))
  (check-prefix-ref trie '() #f #f)
  (check-prefix-ref trie '(1) #f #f)
  (check-prefix-ref trie '(1 2)   'one-two-three-four  '(1 2 3 4))
  (check-prefix-ref trie '(1 3 4) 'one-three-four-five '(1 3 4 5))
  (check-prefix-ref trie '(2)     'two-three           '(2 3))
  (check-prefix-ref trie '(2 3)   'two-three           '(2 3)))

;; trie-set!
(define (check-set trie key value)
  (trie-set! trie key value)
  (check (trie-ref trie key) => value))

(check-set (empty-trie) '()    'null)
(check-set (empty-trie) '(1)   'one)
(check-set (empty-trie) '(1 2) 'one-two)

(check-set (alist->trie '(((1 2 3) . one-two-three))) '() 'null)
(check-set (alist->trie '(((1 2 3) . one-two-three))) '(1) 'one)
(check-set (alist->trie '(((1 2 3) . one-two-three))) '(1 2) 'one-two)
(check-set (alist->trie '(((1 2 3) . one-two-three))) '(1 2 3) 'new-value)
(check-set (alist->trie '(((1 2 3) . one-two-three))) '(1 2 4) 'one-two-four)
(check-set (alist->trie '(((1 2 3) . one-two-three))) '(1 2 3 4) 'one-two-three-four)

;; trie-traverse
(define (check-traverse trie keys expected)
  (check (alist-sort (trie->alist (trie-traverse trie keys))) => (alist-sort expected)))

(let* ((alist '(((1 1 1) . one-one-one)
                ((1 1 2) . one-one-two)
                ((1 2 1) . one-two-one)
                ((2 1)   . two-one)))
       (trie (alist->trie alist)))
  (check-traverse trie '() alist)
  (check-traverse trie '(1) '(((1 1) . one-one-one)
                              ((1 2) . one-one-two)
                              ((2 1) . one-two-one)))
  (check-traverse trie '(1 1) '(((1) . one-one-one)
                                ((2) . one-one-two)))
  (check-traverse trie '(1 1 1) '((() . one-one-one)))
  (check (trie-traverse trie '(1 1 1 1)) => #f)
  (check (trie-traverse trie '(1 1 1 1) #t) => #t))

(end-test "trie")
