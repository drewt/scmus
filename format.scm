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

;;
;; NOTE: this code is pretty inefficient.  It may be worthwhile to
;;       rework cmus's format_print.c and make a wrapper.
;;

(require-extension srfi-1)

(declare (unit format)
         (uses scmus-client)
         (export scmus-format process-format format-string-valid?))

(define (swap pair)
  (assert (pair? pair) "swap" pair)
  (cons (cdr pair) (car pair)))

(define (scmus-state->character state)
  (assert (symbol? state) "scmus-state->character" state)
  (assert (memv state '(play stop pause unknown)) "scmus-state->character" state)
  (case state
    ((play) ">")
    ((stop) ".")
    ((pause) "|")
    ((unknown) "?")))

;; Takes a processed format string (see: process-format) and returns a pair of
;; strings, where the car is the left-justified part and the cdr is the right
;; justified part.
(define (scmus-format fmt len track)
  (assert (list? fmt) "scmus-format" fmt)
  (assert (integer? len) "scmus-format" len)
  (assert (list? track) "scmus-format" track)
  (let ((pair (foldl format-concatenate
                     '("" . "")
                     (map (lambda (x) (format-replace x track len)) fmt))))
    (cons (string-truncate (cdr pair) len #f)
          (string-truncate (car pair) len #t))))

(define (format-concatenate pair e)
  (assert (pair? pair) "format-concatenate" pair)
  (assert (string? (car pair)) "format-concatenate" (car pair))
  (assert (string? (cdr pair)) "format-concatenate" (cdr pair))
  (assert (or (eqv? e 'align) (string? e)) "format-concatenate" e)
  (if (eqv? e 'align)
    (swap pair)
    (cons (string-append (car pair) e) (cdr pair))))

(define (format-replace e track len)
  (assert (list? track) "format-replace" track)
  (assert (integer? len) "format-replace" len)
  (cond
    ((procedure? e) (interp-format-function e track))
    ((or (string? e) (symbol? e)) e)
    ((pair? e) (interp-format-list e track len))
    (else (assert #f "format-replace" e))))

(define (interp-format-function fun track)
  (handle-exceptions e
    (begin (error-set! e) "<error>")
    (format "~a" (fun track))))

(define (interp-format-list l track len)
  (assert (pair? l) "interp-format-list" l)
  (assert (list? track) "interp-format-list" track)
  (assert (integer? len) "interp-format-list" len)
  (let *interp ((rest l) (pad-right #f) (pad-char #\space) (rel #f) (width len))
    (cond
      ((not (pair? rest))
        (string-stretch (format-replace rest track len)
                        pad-char
                        (if rel
                          (integer-scale len width)
                          width)
                        pad-right))
      ((eqv? (car rest) 'pad-right)
        (*interp (cdr rest) #t pad-char rel width))
      ((eqv? (car rest) 'pad-zero)
        (*interp (cdr rest) pad-right #\0 rel width))
      ((eqv? (car rest) 'relative)
        (*interp (cdr rest) pad-right pad-char #t width))
      ((number? (car rest))
        (*interp (cdr rest) pad-right pad-char rel (car rest))))))

(define (format-discnumber track)
  (clean-nr (track-disc track)))
(define (format-tracknumber track)
  (clean-nr (track-track track)))
(define (format-duration track)
  (seconds->string (track-duration track)))
(define (format-playing track)
  (scmus-state->character (scmus-state)))
(define (format-current track)
  (scmus-elapsed))
(define (format-db-playtime track)
  (seconds->string (scmus-db-playtime)))
(define (format-volume track)
  (number->string (scmus-volume)))
(define (format-queue-length track)
  (number->string (scmus-queue-length)))
(define (format-repeat track)
  (if (scmus-repeat?) "R" " "))
(define (format-random track)
  (if (scmus-random?) "r" " "))
(define (format-single track)
  (if (scmus-single?) "S" " "))
(define (format-consume track)
  (if (scmus-consume?) "C" " "))

(define (parse-format-spec spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "parse-format-spec" spec)
  (case (car spec)
    ((#\a) track-artist)
    ((#\l) track-album)
    ((#\A) track-albumartist)
    ((#\D) format-discnumber)
    ((#\n) format-tracknumber)
    ((#\t) track-title)
    ((#\g) track-genre)
    ((#\c) track-comment)
    ((#\y) track-date)
    ((#\d) format-duration)
    ((#\f) track-file)
    ((#\F) track-file)
    ((#\=) 'align)
    ((#\P) format-playing)
    ((#\p) format-current)
    ((#\T) format-db-playtime)
    ((#\v) format-volume)
    ((#\R) format-repeat)
    ((#\r) format-random)
    ((#\S) format-single)
    ((#\C) format-consume)
    ((#\{) (parse-braced-spec (cdr spec)))
    ((#\[) (parse-code-spec (cdr spec)))
    ((#\-) (cons 'pad-right (parse-format-spec (cdr spec))))
    ((#\0) (cons 'pad-zero (parse-format-spec (cdr spec))))
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (parse-numbered-spec spec))))

(define (parse-braced-spec spec)
  (assert (and (list? spec) (not (null? spec))) "parse-braced-spec" spec)
  (case (string->symbol
          (list->string
            (take-while
              (lambda (x) (not (char=? #\} x)))
              spec)))
    ((artist)       track-artist)
    ((album)        track-album)
    ((albumartist)  track-albumartist)
    ((discnumber)   format-discnumber)
    ((tracknumber)  format-tracknumber)
    ((title)        track-title)
    ((genre)        track-genre)
    ((comment)      track-comment)
    ((date)         track-date)
    ((duration)     format-duration)
    ((path)         track-file)
    ((filename)     track-file)
    ((playing)      format-playing)
    ((current)      format-current)
    ((db-playtime)  format-db-playtime)
    ((volume)       format-volume)
    ((queue-length) format-queue-length)
    ((repeat)       format-repeat)
    ((random)       format-random)
    ((single)       format-single)
    ((consume)      format-consume)))

;; Split a code spec <code>]<rest> into (values <code> <rest>), handling
;; escaped characters in <code>.
(define (code-split spec)
  (let loop ((rest spec) (code '()) (escaping? #f))
    (cond
      ((null? rest) (values (reverse code) #f))
      (escaping? (loop (cdr rest) (cons (car rest) code) #f))
      (else
        (case (car rest)
          ((#\])  (values (reverse code) (cdr rest)))
          ((#\\ ) (loop (cdr rest) code #t))
          (else   (loop (cdr rest) (cons (car rest) code) #f)))))))

(define (parse-code-spec spec)
  (assert (and (list? spec) (not (null? spec))) "parse-code-spec" spec)
  (let* ((str (list->string (code-split spec)))
         (obj (user-eval str)))
    (if (procedure? obj)
      obj
      (lambda (x) obj))))

(define (parse-numbered-spec spec)
  (assert (and (list? spec) (not (null? spec))) "parse-numbered-spec" spec)
  (define (*parse-spec spec n)
    (cond
      ((char-numeric? (car spec))
        (*parse-spec (cdr spec)
                     (+ (* n 10)
                        (- (char->integer (car spec))
                           (char->integer #\0)))))
      ((char=? (car spec) #\%)
        (cons 'relative (cons n (parse-format-spec (cdr spec)))))
      (else (cons n (parse-format-spec spec)))))
  (*parse-spec spec 0))

;; skips over a format spec in a char list
(define (format-next spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "format-next" spec)
  (cond
    ((char=? (car spec) #\{) (braced-next (cdr spec)))
    ((char=? (car spec) #\[) (code-next (cdr spec)))
    ((char=? (car spec) #\-) (format-next (cdr spec)))
    ((char-numeric? (car spec)) (numbered-next spec))
    (else (cdr spec))))

(define (braced-next spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "braced-next" spec)
  (if (char=? (car spec) #\})
    (cdr spec)
    (braced-next (cdr spec))))

(define (code-next spec)
  (nth-value 1 (code-split spec)))

(define (skip-number spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "skip-number" spec)
  (if (char-numeric? (car spec))
    (skip-number (cdr spec))
    spec))

(define (numbered-next spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "numbered-next" spec)
  (let ((rest (skip-number spec)))
    (if (char=? (car rest) #\%)
      (format-next (cdr rest))
      (format-next rest))))

(define (format-spec-valid? spec)
  (assert (list? spec) "format-spec-valid?" spec)
  (if (null? spec)
    #f
    (case (car spec)
      ((#\a #\A #\l #\D #\n #\t #\g #\c #\y #\d #\f #\F #\~ #\= #\P #\p #\T #\v
        #\R #\r #\S #\C) #t)
      ((#\{) (braced-spec-valid? (cdr spec)))
      ((#\[) (code-spec-valid? (cdr spec)))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) (numbered-spec-valid? spec))
      ((#\-) (format-spec-valid? (cdr spec)))
      (else #f))))

(define (braced-spec-valid? spec)
  (memv (string->symbol
          (list->string
            (take-while
              (lambda (x) (not (char=? #\} x)))
              spec)))
        '(artist
          album
          albumartist
          discnumber
          tracknumber
          title
          genre
          comment
          date
          duration
          path
          filename
          playing
          current
          db-playtime
          volume
          queue-length
          repeat
          random
          single
          consume)))

(define (code-spec-valid? spec)
  (condition-case
    (let* ((str (list->string (code-split spec)))
           (body (read (open-input-string str))))
      #t)
    (e () (error-set! e) #f)))

(define (numbered-spec-valid? spec)
  (format-spec-valid? (skip-number spec)))

;; this should be called on any user-entered format string
(define (format-string-valid? chars)
  (assert (list? chars) "format-string-valid?" chars)
  (cond
    ((null? chars) #t)
    ((not (char=? (car chars) #\~))
      (format-string-valid? (cdr chars)))
    ((format-spec-valid? (cdr chars))
      (format-string-valid? (format-next (cdr chars))))
    (else #f)))

;; replaces format specifiers with symbols.
;; chars is assumed valid.
(define (process-format chars)
  (assert (list? chars) "process-format" chars)
  ; first pass: parse format specifiers from list of chars
  (define (parse-format in)
    (let loop ((in in) (out '()))
      (cond
        ((null? in)
          ; if the format string didn't contain "~=", 'align is added at the end
          (reverse (if (member 'align out)
                     out
                     (cons 'align out))))
        ((not (char=? (car in) #\~))
          (loop (cdr in) (cons (car in) out)))
        (else
          (loop (format-next (cdr in))
                (cons (parse-format-spec (cdr in)) out))))))
  ; second pass: convert consecutive chars to strings
  (define (stringify-format fmt)
    (let loop ((rest (cdr fmt))
               (last (if (char? (car fmt)) (string (car fmt)) (car fmt)))
               (rv '()))
      (cond
        ((null? rest) (reverse (cons last rv)))
        ((and (string? last) (char? (car rest)))
          (loop (cdr rest) (string-append last (string (car rest))) rv))
        ((char? (car rest))
          (loop (cdr rest) (string (car rest)) (cons last rv)))
        (else
          (loop (cdr rest) (car rest) (cons last rv))))))
  (stringify-format (parse-format chars)))
