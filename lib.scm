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

(require-extension srfi-13 ncurses)

(declare (unit lib)
         (uses config
               ui-curses))

;; the exit routine; initially (exit), becomes a continuation
(define scmus-exit exit)

(define *scmus-error* "")

(define (verbose-printf . args)
  (if *verbose*
    (apply console-printf args)))

(define (debug-printf . args)
  (if *debug*
    (apply console-printf args)))

(define (debug-pp sexp)
  (if *debug*
    (console-pp sexp)))

(define (console-printf . args)
  (if *ui-initialized*
    (begin
      (endwin)
      (apply printf args)
      (refresh))
    (apply printf args)))

(define (console-pp sexp)
  (if *ui-initialized*
    (begin
      (endwin)
      (pp sexp)
      (refresh))
    (pp sexp)))

;; Equality predicate for characters and ncurses keycodes.
;; This is necessary because the ncurses egg has KEY_* constants as integers
;; for some reason.
(define (key= ch key)
  (assert (integer? key))
  (char=? ch (integer->char key)))

;; #t if ch is not an ascii character
(define (key? ch)
  (assert (char? ch))
  (> (char->integer ch) 255))

(define (string-split-lines str)
  (string-tokenize str (char-set-filter (lambda (c)
                                          (not (char=? c #\newline)))
                                        char-set:full)))

(define (string-truncate s len #!optional (left #f))
  (assert (string? s))
  (assert (integer? len))
  (if (> (string-length s) len)
    (list->string ((if left take-right take) (string->list s) len))
    s))

(define (string-stretch str c len #!optional (right #f))
  (assert (string? str))
  (assert (char? c))
  (assert (integer? len))
  (if (> len (string-length str))
    (if right
      (string-pad-right str len c)
      (string-pad str len c))
    (string-truncate str len)))

(define (integer-scale len percent)
  (assert (integer? len))
  (assert (integer? percent))
  (assert (>= len 0))
  (assert (>= percent 0))
  (inexact->exact (round (* len (/ percent 100)))))

(define (seconds->string total-seconds)
  (assert (integer? total-seconds))
  (assert (>= total-seconds 0))
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

(define (clean-nr str)
  (assert (string? str))
  (let ((i (string-index str #\/)))
    (if i (string-take str i) str)))

(define (error-set! error)
  (let ((out (open-output-string)))
    (pretty-print (condition->list error) out)
    (set! *scmus-error* (get-output-string out)))
  (register-event! 'error-changed))
