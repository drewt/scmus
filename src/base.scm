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

(module scmus.base *
  (import (chicken module))
  (reexport
    (except scheme
            ; XXX: provided by utf8 egg
            string-length
            string-ref
            string-set!
            make-string
            string
            substring
            string->list
            list->string
            string-fill!
            write-char
            read-char
            display)
    (only (chicken base)
          ; chicken-specific functions
          alist-ref
          alist-update
          alist-update!
          assert
          define-constant
          getter-with-setter
          nth-value
          rec
          setter
          void
          let-optionals*
          optional
          ; R7RS
          case-lambda
          current-error-port
          define-record-type
          error
          get-output-string
          let-values
          let*-values
          make-parameter
          open-input-string
          open-output-string
          parameterize
          when
          unless)
    (chicken bitwise)
    (chicken condition)
    (only (chicken format) format)
    (chicken sort)
    (chicken type)
    (srfi 1)
    utf8
    (except utf8-srfi-13 string-contains-ci)
    utf8-srfi-14)

  (import (chicken time))

  ; Use the R7RS names
  (define raise abort)
  (define raise-continuable signal)
  (define current-second current-seconds)

  (define (current-second/precise)
    (/ (current-milliseconds) 1000.0))

  ;; XXX: utf8-srfi-13#string-contains-ci isn't case-insensitive...
  (define (string-contains-ci str sub)
    (string-contains (string-downcase str) (string-downcase sub)))

  (: integer-scale (fixnum fixnum -> fixnum))
  (define (integer-scale len percent)
    (assert (>= len 0) "integer-scale" len)
    (inexact->exact (round (* len (/ percent 100))))))
