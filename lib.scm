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

(declare (unit lib)
         (uses config))

;; the exit routine; initially (exit), becomes a continuation
(define scmus-exit exit)

(define (verbose-printf . args)
  (if *verbose*
    (apply printf args)))

;; Equality predicate for characters and ncurses keycodes.
;; This is necessary because the ncurses egg has KEY_* constants as integers
;; for some reason.
(define (key= ch key)
  (eqv? ch (integer->char key)))

;; #t if ch is not a printable character
(define (key? ch)
  (> (char->integer ch) 255))

(define (string-truncate s len)
  (if (> (string-length s) len)
    (list->string (take (string->list s) len))
    s))

(define (string-truncate-left s len)
  (if (> (string-length s) len)
    (list->string (take-right (string->list s) len))
    s))

(define (seconds->string total-seconds)
  (let* ((total-minutes (quotient total-seconds 60))
         (seconds (modulo total-seconds 60))
         (minutes (modulo total-minutes 60))
         (hours (quotient total-minutes 60)))
    (string-append (if (= hours 0)
                     ""
                     (format "~a:" hours))
                   (if (< minutes 10)
                     (format "0~a:" minutes)
                     (format "~a:" minutes))
                   (if (< seconds 10)
                     (format "0~a" seconds)
                     (number->string seconds)))))
