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

(declare (unit error)
         (uses command-line event))

(module scmus.error (scmus-error scmus-error-set!)
  (import scmus.base scmus.command-line scmus.event)

  (: *scmus-error* string)
  (define *scmus-error* "")

  (: scmus-error (-> string))
  (define (scmus-error) *scmus-error*)

  (: scmus-error-set! (condition -> undefined))
  (define (scmus-error-set! error)
    (let ((out (open-output-string)))
      (pretty-print (condition->list error) out)
      (verbose-printf "~a~n"(get-output-string out))
      (set! *scmus-error* (get-output-string out)))
    (if ((condition-predicate 'exn) error)
      (command-line-print-error!
        (format "~a: ~s" (get-condition-property error 'exn 'message)
                         (get-condition-property error 'exn 'arguments))))
    (register-event! 'error-changed)))
