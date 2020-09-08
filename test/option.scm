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
(declare (unit test/option)
         (uses option
               event))

(import scmus.option
        scmus.event)

(start-test "option")

;; validator
(let ((opt (make-option 1 'validator integer?)))
  (check (option-value opt) => 1)
  (set! (option-value opt) 1.5)
  (check (option-value opt) => 1)
  (set! (option-value opt) 2)
  (check (option-value opt) => 2))

;; converter
(let ((opt (make-option -1 'converter abs)))
  (check (option-value opt) => 1))

;; after
(let* ((n 0)
       (opt (make-option 1 'after (lambda (_) (set! n (+ n 1))))))
  (check (= n 1) => #t)
  (set! (option-value opt) 2)
  (check (= n 2) => #t))

;; event
(let ((opt (make-option 1))
      (ran? #f))
  (add-listener opt 'option-changed (lambda (o)
                                      (check (option-value o) => 2)
                                      (set! ran? #t)))
  (set! (option-value opt) 2)
  (handle-events)
  (check ran? => #t))

(end-test "option")
