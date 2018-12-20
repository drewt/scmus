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

(module scmus.base *
  (reexport
    (except scheme
      string-length string-ref string-set! make-string string substring
      string->list list->string string-fill! write-char read-char display)
    (except chicken
      reverse-list->string print print*)
    (except data-structures
      conc string-chop string-split string-translate
      substring=? substring-ci=? substring-index substring-index-ci)
    (except extras
      read-string write-string read-token)
    srfi-1
    utf8
    (except utf8-srfi-13 string-contains-ci)
    utf8-srfi-14)

  ;; XXX: utf8-srfi-13#string-contains-ci isn't case-insensitive...
  (define (string-contains-ci str sub)
    (string-contains (string-downcase str) (string-downcase sub)))

  (: integer-scale (fixnum fixnum -> fixnum))
  (define (integer-scale len percent)
    (assert (>= len 0) "integer-scale" len)
    (inexact->exact (round (* len (/ percent 100))))))
