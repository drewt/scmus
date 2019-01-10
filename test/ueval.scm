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
(declare (unit test/ueval)
         (uses ueval))

(import scmus.ueval)

(start-test "ueval")

;; eval
(check (user-eval '(+ 1 1)) => 2)
(check (user-eval-string "(+ 1 1)") => 2)

;; load
(user-load "test/ueval-load.scm")
(check (user-eval 'defined-as-two) => 2)

;; export
(user-export! 'exported-as-two 2)
(check (user-eval 'exported-as-two) => 2)

;; user-value-set!
(user-value-set! 'set!-as-two 2 "The number two")
(check (user-value-ref 'set!-as-two) => 2)
(check (user-doc-ref 'set!-as-two) => "The number two")

(end-test "ueval")
