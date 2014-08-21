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

;; Takes a processed format string (see: process-format)
;; and returns a pair of strings, where the car is the
;; left-justified part and the cdr is the right justified
;; part.
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
  (assert (or (symbol? e) (string? e)) "format-concatenate" e)
  (if (symbol? e)
    (swap pair)
    (cons (string-append (car pair) e) (cdr pair))))

(define (format-replace e track len)
  (assert (or (symbol? e) (pair? e) (char? e)) "format-replace" e)
  (assert (list? track) "format-replace" track)
  (assert (integer? len) "format-replace" len)
  (cond
    ((symbol? e) (interp-format-symbol e track))
    ((pair? e) (interp-format-list e track len))
    ((char? e) (string e))))

(define (interp-format-symbol e track)
  (assert (symbol? e) "interp-format-symbol" e)
  (assert (list? track) "interp-format-symbol" track)
  (assert (memv e '(artist album albumartist discnumber tracknumber title
                    genre comment date duration path filename align playing
                    current db-playtime volume queue-length repeat random
                    single consume)) "interp-format-symbol" e)
  (case e
    ((artist) (track-artist track))
    ((album) (track-album track))
    ((albumartist) (track-albumartist track))
    ((discnumber) (clean-nr (track-disc track)))
    ((tracknumber) (clean-nr (track-track track)))
    ((title) (track-title track))
    ((genre) (track-genre track))
    ((comment) (track-comment track))
    ((date) (track-date track))
    ((duration) (seconds->string (track-duration track)))
    ((path) (track-file track))
    ((filename) (track-file track)) ; FIXME: need to extract filename
    ((align) 'align)
    ((playing) (scmus-state->character (scmus-state)))
    ((current) (scmus-elapsed))
    ((db-playtime) (seconds->string (scmus-db-playtime)))
    ((volume) (number->string (scmus-volume)))
    ((queue-length) (number->string (scmus-queue-length)))
    ((repeat) (if (scmus-repeat?) "R" " "))
    ((random) (if (scmus-random?) "r" " "))
    ((single) (if (scmus-single?) "S" " "))
    ((consume) (if (scmus-consume?) "C" " "))))

(define (interp-format-list l track len)
  (assert (pair? l) "interp-format-list" l)
  (assert (list? track) "interp-format-list" track)
  (assert (integer? len) "interp-format-list" len)
  (let *interp ((rest l) (pad-right #f) (pad-char #\space) (rel #f) (width len))
    (cond
      ((symbol? rest)
        (string-stretch (interp-format-symbol rest track)
                        pad-char
                        (if rel
                          (integer-scale len width)
                          width)
                        pad-right))
      ((eqv? (car rest) 'function)
        (let ((r ((cdr rest) track)))
          (if (string? r)
            r
            (format "<error: not a string: ~a>" r))))
      ((eqv? (car rest) 'pad-right)
        (*interp (cdr rest) #t pad-char rel width))
      ((eqv? (car rest) 'pad-zero)
        (*interp (cdr rest) pad-right #\0 rel width))
      ((eqv? (car rest) 'relative)
        (*interp (cdr rest) pad-right pad-char #t width))
      ((number? (car rest))
        (*interp (cdr rest) pad-right pad-char rel (car rest))))))

(define (parse-format-spec spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec)))
          "parse-format-spec" spec)
  (case (car spec)
    ((#\a) 'artist)
    ((#\l) 'album)
    ((#\A) 'albumartist)
    ((#\D) 'discnumber)
    ((#\n) 'tracknumber)
    ((#\t) 'title)
    ((#\g) 'genre)
    ((#\c) 'comment)
    ((#\y) 'date)
    ((#\d) 'duration)
    ((#\f) 'path)
    ((#\F) 'filename)
    ((#\=) 'align)
    ((#\P) 'playing)
    ((#\p) 'current)
    ((#\T) 'db-playtime)
    ((#\v) 'volume)
    ((#\R) 'repeat)
    ((#\r) 'random)
    ((#\S) 'single)
    ((#\C) 'consume)
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
    ((artist)       'artist)
    ((album)        'album)
    ((albumartist)  'albumartist)
    ((discnumber)   'discnumber)
    ((tracknumber)  'tracknumber)
    ((title)        'title)
    ((genre)        'genre)
    ((comment)      'comment)
    ((date)         'date)
    ((duration)     'duration)
    ((path)         'path)
    ((filename)     'filename)
    ((playing)      'playing)
    ((current)      'current)
    ((db-playtime)  'db-playtime)
    ((volume)       'volume)
    ((queue-length) 'queue-length)
    ((repeat)       'repeat)
    ((random)       'random)
    ((single)       'single)
    ((consume)      'consume)))

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
         (fun (user-eval str)))
    (if (procedure? fun)
      (cons 'function fun)
      (cons 'function (lambda (x) "<error: not a function>")))))

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

;; Considered valid if contains a valid s-expression.  We don't check that it
;; evaluates to a function, in case it has side-effects.
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
  (define (*process-format in out)
    (cond
      ((null? in)
        out)
      ((not (char=? (car in) #\~))
        (*process-format (cdr in)
                         (cons (car in) out)))
      (else
        (*process-format (format-next (cdr in))
                         (cons (parse-format-spec (cdr in)) out)))))
  ; if the format string didn't contain "~=", 'align is added at the end
  (let ((processed (*process-format chars '())))
    (reverse (if (member 'align processed)
               processed
               (cons 'align processed)))))
