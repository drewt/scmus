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

(declare (unit lib))

;; the exit routine; initially (exit), becomes a continuation
(define scmus-exit exit)

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