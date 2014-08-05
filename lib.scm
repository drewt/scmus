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

(define *scmus-error* "")

(define *views*
  '(library queue status error options))

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

;; #t if ch is not an ascii character
(define (key? ch)
  (assert (char? ch))
  (> (char->integer ch) 255))

;; unicode stuff {{{

(define (string-width str)
  (foldl + 0 (map char-width (string->list str))))

(define (char-width c)
  (let ((u (char->integer c)))
    (cond
      ((< u #x20) 0)
      ((< u #x1100) 1)
      ; Hangul Jamo init. consonants
      ((<= u #x115f) 2)
      ; Zero-width characters
      ((or (= u #x200b) (= u #x200c) (= u #x200d)) 0)
      ; Angle brackets
      ((or (= u #x2329) (= u #x232a)) 2)
      ((< u #x2e80) 1)
      ; CJK ... Yi
      ((< u #x302a) 2)
      ((<= u #x302f) 1)
      ((= u #x303f) 1)
      ((= u #x3099) 1)
      ((= u #x309a) 1)
      ; CJK ... Yi
      ((<= u #xa4cf) 2)
      ; Hangul Syllables
      ((and (>= u #xac00) (<= u #xd7a3)) 2)
      ; CJK Compatibility Ideographs
      ((and (>= u #xf900) (<= u #xfaff)) 2)
      ; CJK Compatibility Forms
      ((and (>= u #xfe30) (<= u #xfe6f)) 2)
      ; Fullwidth Forms
      ((and (>= u #xff00) (<= u #xff60)) 2)
      ((and (>= u #xffe0) (<= u #xffe6)) 2)
      ; CJK Extra Stuff
      ((and (>= u #x20000) (<= u #x2fffd)) 2)
      ; ?
      ((and (>= u #x30000) (<= u #x3fffd)) 2)
      (else 1))))

(define (ustring-take str width)
  (let loop ((result '()) (rest (string->list str)) (r-width 0))
    (let* ((c (car rest))
           (c-width (char-width c)))
      (if (> (+ r-width c-width) width)
        (list->string (reverse result))
        (loop (cons c result) (cdr rest) (+ r-width c-width))))))

(define (ustring-take-right str width)
  (let loop ((result '()) (rest (reverse (string->list str))) (r-width 0))
    (let* ((c (car rest))
           (c-width (char-width c)))
      (if (> (+ r-width c-width) width)
        (list->string result)
        (loop (cons c result) (cdr rest) (+ r-width c-width))))))

(define (string-truncate s len #!optional (left #f))
  (let ((width (string-width s)))
    (if (> width len)
      (if left
        (ustring-take-right s len)
        (ustring-take s len))
      s)))

(define (ustring-pad str len c #!optional (right #f))
  (if right
    (string-append str (make-string (- len (string-width str)) c))
    (string-append (make-string (- len (string-width str)) c) str)))

(define (string-stretch str c len #!optional (right #f))
  (assert (string? str))
  (assert (char? c))
  (assert (integer? len))
  (if (> len (string-width str))
    (ustring-pad str len c right)
    (string-truncate str len)))

(define (integer-scale len percent)
  (assert (integer? len))
  (assert (integer? percent))
  (assert (>= len 0))
  (inexact->exact (round (* len (/ percent 100)))))

(define (string-split-lines str)
  (let loop ((result '()) (substr '()) (rest (string->list str)))
    (if (null? rest)
      (if (null? substr)
        (reverse result)
        (reverse (cons (list->string (reverse substr)) result)))
      (if (char=? (car rest) #\newline)
        (loop (cons (list->string (reverse substr)) result) '() (cdr rest))
        (loop result (cons (car rest) substr) (cdr rest))))))

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
