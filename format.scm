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
         (export scmus-format
                 process-format
                 format-string-valid?))

(define (swap pair)
  (assert (pair? pair))
  (cons (cdr pair) (car pair)))

(define (ascii-num? c)
  (assert (char? c))
  (memv c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(define (scmus-state->character state)
  (assert (symbol? state))
  (assert (memv state '(play stop pause unknown)))
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
  (assert (list? fmt))
  (assert (integer? len))
  (assert (list? track))
  (let ((pair (foldl format-concatenate
                     '("" . "")
                     (map (lambda (x) (format-replace x track len)) fmt))))
    (cons (string-truncate (cdr pair) len #f)
          (string-truncate (car pair) len #t))))

(define (format-concatenate pair e)
  (assert (pair? pair))
  (if (symbol? e)
    (swap pair)
    (cons (string-append (car pair) e) (cdr pair))))

(define (format-replace e track len)
  (assert (or (symbol? e) (pair? e) (char? e)))
  (assert (list? track))
  (assert (integer? len))
  (cond
    ((symbol? e) (interp-format-symbol e track))
    ((pair? e) (interp-format-list e track len))
    ((char? e) (string e))))

(define (interp-format-symbol e track)
  (assert (symbol? e))
  (assert (list? track))
  (assert (memv e '(artist album albumartist discnumber tracknumber title
                    genre comment date duration path filename align playing
                    current db-playtime volume queue-length repeat random
                    single consume)))
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
    ((path) (track-file track)) ; FIXME: need to prepend mpd music dir
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
  (assert (pair? l))
  (assert (list? track))
  (assert (integer? len))
  (let *interp ((rest l) (pad-right #f) (pad-char #\space) (rel #f) (width len))
    (cond
      ((symbol? rest)
        (string-stretch (interp-format-symbol rest track)
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

(define (parse-format-spec spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec))))
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
    ((#\-) (cons 'pad-right (parse-format-spec (cdr spec))))
    ((#\0) (cons 'pad-zero (parse-format-spec (cdr spec))))
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (parse-numbered-spec spec))))

(define (parse-braced-spec spec)
  (assert (list? spec))
  (assert (not (null? spec)))
  (case (string->symbol
          (list->string
            (take-while
              (lambda (x) (not (eqv? #\} x)))
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

(define (parse-numbered-spec spec)
  (assert (and (list? spec) (not (null? spec))))
  (define (*parse-spec spec n)
    (cond
      ((ascii-num? (car spec))
        (*parse-spec (cdr spec)
                     (+ (* n 10)
                        (- (char->integer (car spec))
                           (char->integer #\0)))))
      ((eqv? (car spec) #\%)
        (cons 'relative (cons n (parse-format-spec (cdr spec)))))
      (else (cons n (parse-format-spec spec)))))
  (*parse-spec spec 0))

;; skips over a format spec in a char list
(define (format-next spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec))))
  (cond
    ((eqv? (car spec) #\{) (braced-next (cdr spec)))
    ((eqv? (car spec) #\-) (format-next (cdr spec)))
    ((ascii-num? (car spec)) (numbered-next spec))
    (else (cdr spec))))

(define (braced-next spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec))))
  (if (eqv? (car spec) #\})
    (cdr spec)
    (braced-next (cdr spec))))

(define (skip-number spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec))))
  (if (ascii-num? (car spec))
    (skip-number (cdr spec))
    spec))

(define (numbered-next spec)
  (assert (and (list? spec) (not (null? spec)) (char? (car spec))))
  (let ((rest (skip-number spec)))
    (if (eqv? (car rest) #\%)
      (format-next (cdr rest))
      (format-next rest))))

(define (format-spec-valid? spec)
  (assert (list? spec))
  (if (null? spec)
    #f
    (case (car spec)
      ((#\a #\A #\l #\D #\n #\t #\g #\c #\y #\d #\f #\F #\~ #\= #\P #\p #\T #\v
        #\R #\r #\S #\C) #t)
      ((#\{) (braced-spec-valid? (cdr spec)))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) (numbered-spec-valid? spec))
      ((#\-) (format-spec-valid? (cdr spec)))
      (else #f))))

(define (braced-spec-valid? spec)
  (memv (string->symbol
          (list->string
            (take-while
              (lambda (x) (not (eqv? #\} x)))
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

(define (numbered-spec-valid? spec)
  (format-spec-valid? (skip-number spec)))

;; this should be called on any user-entered format string
(define (format-string-valid? chars)
  (assert (list? chars))
  (cond
    ((null? chars) #t)
    ((not (eqv? (car chars) #\~))
      (format-string-valid? (cdr chars)))
    ((format-spec-valid? (cdr chars))
      (format-string-valid? (format-next (cdr chars))))
    (else #f)))

;; replaces format specifiers with symbols.
;; chars is assumed valid.
(define (process-format chars)
  (assert (list? chars))
  (define (*process-format in out)
    (cond
      ((null? in)
        out)
      ((not (eqv? (car in) #\~))
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
