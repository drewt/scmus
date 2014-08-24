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
         (uses option scmus-client)
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
  (let ((r (*scmus-format fmt len track)))
    (if (string? r)
      (string-truncate r len)
      (let* ((right-len (min (string-length (cdr r)) len))
             (left-len (- len right-len)))
        (string-append (string-stretch (car r) #\space left-len #t)
                       (string-stretch (cdr r) #\space right-len))))))

(define (*scmus-format fmt len track)
  (foldl format-concatenate
         ""
         (map (lambda (x) (format-replace x track len)) fmt)))

(define (format-concatenate acc x)
  (assert (or (pair? acc) (string? acc)) "format-concatenate" acc)
  (cond
    ((and (string? acc) (eqv? x 'align))
      (cons acc ""))
    ((string? acc)
      (string-append acc x))
    ((string? x)
      (cons (car acc) (string-append (cdr acc) x)))
    (else acc)))

(define (format-replace e track len)
  (assert (list? track) "format-replace" track)
  (assert (integer? len) "format-replace" len)
  (cond
    ((procedure? e) (interp-format-function e track len))
    ((or (string? e) (symbol? e)) e)
    ((pair? e) (interp-format-list e track len))
    (else (assert #f "format-replace" e))))

(define (interp-format-function fun track len)
  (handle-exceptions e
    (begin (error-set! e) "<error>")
    (format "~a" (fun track len))))

(define (interp-format-list l track len)
  (assert (pair? l) "interp-format-list" l)
  (assert (list? track) "interp-format-list" track)
  (assert (integer? len) "interp-format-list" len)
  (let *interp ((rest l) (pad-right #f) (pad-char #\space) (rel #f) (width len))
    (cond
      ((not (pair? rest))
        (let ((width (if rel (integer-scale len width) width)))
          (string-stretch (format-replace rest track width)
                          pad-char
                          width
                          pad-right)))
      ((eqv? (car rest) 'pad-right)
        (*interp (cdr rest) #t pad-char rel width))
      ((eqv? (car rest) 'pad-zero)
        (*interp (cdr rest) pad-right #\0 rel width))
      ((eqv? (car rest) 'relative)
        (*interp (cdr rest) pad-right pad-char #t width))
      ((number? (car rest))
        (*interp (cdr rest) pad-right pad-char rel (car rest))))))

(define (format-artist track len)
  (track-artist track))
(define (format-album track len)
  (track-album track))
(define (format-albumartist track len)
  (track-albumartist track))
(define (format-discnumber track len)
  (clean-nr (track-disc track)))
(define (format-tracknumber track len)
  (clean-nr (track-track track)))
(define (format-title track len)
  (track-title track))
(define (format-genre track len)
  (track-genre track))
(define (format-comment track len)
  (track-comment track))
(define (format-date track len)
  (track-date track))
(define (format-duration track len)
  (seconds->string (track-duration track)))
(define (format-path track len)
  (track-file track))
(define (format-filename track len)
  (track-file track))
(define (format-playing track len)
  (scmus-state->character (scmus-state)))
(define (format-current track len)
  (scmus-elapsed-string))
(define (format-db-playtime track len)
  (seconds->string (scmus-db-playtime)))
(define (format-volume track len)
  (number->string (scmus-volume)))
(define (format-queue-length track len)
  (number->string (scmus-queue-length)))
(define (format-repeat track len)
  (if (scmus-repeat?) "R" " "))
(define (format-random track len)
  (if (scmus-random?) "r" " "))
(define (format-single track len)
  (if (scmus-single?) "S" " "))
(define (format-consume track len)
  (if (scmus-consume?) "C" " "))

(define (parse-format-spec spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "parse-format-spec" spec)
  (case (car spec)
    ((#\a) format-artist)
    ((#\l) format-album)
    ((#\A) format-albumartist)
    ((#\D) format-discnumber)
    ((#\n) format-tracknumber)
    ((#\t) format-title)
    ((#\g) format-genre)
    ((#\c) format-comment)
    ((#\y) format-date)
    ((#\d) format-duration)
    ((#\f) format-path)
    ((#\F) format-filename)
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
    ((#\<) (parse-option-spec (cdr spec)))
    ((#\-) (cons 'pad-right (parse-format-spec (cdr spec))))
    ((#\0) (cons 'pad-zero (parse-format-spec (cdr spec))))
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (parse-numbered-spec spec))))

(define (take-until->symbol chars delim)
  (string->symbol
    (list->string
      (take-while
        (lambda (x) (not (char=? x delim)))
        chars))))

(define (parse-braced-spec spec)
  (assert (and (list? spec) (not (null? spec))) "parse-braced-spec" spec)
  (let ((meta (take-until->symbol spec #\})))
   (case meta
    ((artist)       format-artist)
    ((album)        format-album)
    ((albumartist)  format-albumartist)
    ((discnumber)   format-discnumber)
    ((tracknumber)  format-tracknumber)
    ((title)        format-title)
    ((genre)        format-genre)
    ((comment)      format-comment)
    ((date)         format-date)
    ((duration)     format-duration)
    ((path)         format-path)
    ((filename)     format-filename)
    ((playing)      format-playing)
    ((current)      format-current)
    ((db-playtime)  format-db-playtime)
    ((volume)       format-volume)
    ((queue-length) format-queue-length)
    ((repeat)       format-repeat)
    ((random)       format-random)
    ((single)       format-single)
    ((consume)      format-consume)
    ((host)         (lambda (track len) (scmus-hostname)))
    ((port)         (lambda (track len) (scmus-port)))
    (else           (lambda (track len) (track-meta track meta))))))

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
      (lambda (track len) obj))))

(define (parse-option-spec spec)
  (assert (and (list? spec) (not (null? spec))) "parse-option-spec" spec)
  (let ((name (take-until->symbol spec #\>)))
    (lambda (track len) (get-option name))))

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
    ((char=? (car spec) #\<) (option-next (cdr spec)))
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
  (cond
    ((char-numeric? (car spec)) (skip-number (cdr spec)))
    ((char=? (car spec) #\%) (cdr spec))
    (else spec)))

(define (option-next spec)
  (if (char=? (car spec) #\>)
    (cdr spec)
    (option-next (cdr spec))))

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
      ((#\<) (option-spec-valid? (cdr spec)))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) (numbered-spec-valid? spec))
      ((#\-) (format-spec-valid? (cdr spec)))
      (else #f))))

(define (braced-spec-valid? spec)
  (memv #\} spec))

(define (code-spec-valid? spec)
  (let-values (((code rest) (code-split spec)))
    (handle-exceptions e
      (begin (error-set! e) #f)
      (read (open-input-string (list->string code)))
      rest)))

(define (option-spec-valid? spec)
  (memv #\> spec))

(define (numbered-spec-valid? spec)
  (format-spec-valid? (skip-number spec)))

;; this should be called on any user-entered format string
(define (format-string-valid? str)
  (assert (string? str) "format-string-valid?" str)
  (let loop ((chars (string->list str)))
    (cond
      ((null? chars) #t)
      ((not (char=? (car chars) #\~))
        (loop (cdr chars)))
      ((format-spec-valid? (cdr chars))
        (loop (format-next (cdr chars))))
      (else #f))))

;; replaces format specifiers with symbols.
;; str is assumed valid.
(define (process-format str)
  (assert (string? str) "process-format" str)
  ; first pass: parse format specifiers from list of chars
  (define (parse-format in)
    (let loop ((in in) (out '()))
      (cond
        ((null? in)
          (reverse out))
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
  (stringify-format (parse-format (string->list str))))
