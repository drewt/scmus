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

;;
;; NOTE: this code is pretty inefficient.  It may be worthwhile to
;;       rework cmus's format_print.c and make a wrapper.
;;

(module scmus.format (scmus-format
                      process-format
                      format-string-valid?)
  (import irregex
          drewt.ncurses
          drewt.ustring
          scmus.base
          scmus.error
          scmus.ueval
          scmus.status
          scmus.track)

  (: *->color-code (* -> fixnum))
  (define (*->color-code x)
    (cond
      ((string? x) (or (*->color-code (string->number x))
                       (*->color-code (string->symbol x))))
      ((and (integer? x) (>= x -1) (< x 256)) x)
      (else (case x
              ((reset !)       -2)
              ((default)       -1)
              ((black)         COLOR_BLACK)
              ((red)           COLOR_RED)
              ((green)         COLOR_GREEN)
              ((yellow)        COLOR_YELLOW)
              ((blue)          COLOR_BLUE)
              ((magenta)       COLOR_MAGENTA)
              ((cyan)          COLOR_CYAN)
              ((white)         COLOR_WHITE)
              ((dark-gray)     8)
              ((light-red)     9)
              ((light-green)   10)
              ((light-yellow)  11)
              ((light-blue)    12)
              ((light-magenta) 13)
              ((light-cyan)    14)
              ((gray)          15)
              (else            #f)))))

  (: scmus-state-character (symbol -> string))
  (define (scmus-state->character state)
    (assert (memv state '(play stop pause unknown)) "scmus-state->character" state)
    (case state
      ((play) ">")
      ((stop) ".")
      ((pause) "|")
      ((unknown) "?")))

  ;; Takes a processed format string (see: process-format) and returns a pair of
  ;; strings, where the car is the left-justified part and the cdr is the right
  ;; justified part.
  (: scmus-format (list fixnum track -> string))
  (define (scmus-format fmt len track)
    (let-values (((left center right) (*scmus-format fmt len track)))
      (if (string=? center "")
        (let* ((right-len (min (string-width right) len))
               (left-len  (- len right-len)))
          (string-append (string-stretch left  #\space left-len #t)
                         (string-stretch right #\space right-len)))
        (let* ((center-len (min (string-width center) len))
               (center-pos (- (quotient len 2)
                              (quotient center-len 2)))
               (right-len  (- len (+ center-pos center-len)))
               (left-len   center-pos))
          (string-append (string-stretch left   #\space left-len #t)
                         (string-stretch center #\space center-len)
                         (string-stretch right  #\space right-len))))))

  (: *scmus-format (list fixnum track -> *))
  (define (*scmus-format fmt len track)
    (let loop ((fmt (map (lambda (x) (format-replace x track len)) fmt))
               (acc (make-vector 3 ""))
               (idx 0))
      (cond
        ((null? fmt)
          (values (vector-ref acc 0)
                  (vector-ref acc 1)
                  (vector-ref acc 2)))
        ((eqv? (car fmt) 'align-center)
          (loop (cdr fmt) acc 1))
        ((eqv? (car fmt) 'align-right)
          (loop (cdr fmt) acc 2))
        ((string? (car fmt))
          (vector-set! acc idx (string-append (vector-ref acc idx) (car fmt)))
          (loop (cdr fmt) acc idx))
        (else
          (loop (cdr fmt) acc idx)))))

  (: format-replace ((or (track fixnum -> *) (or string symbol) pair)
                     track
                     fixnum
                       -> (or string symbol)))
  (define (format-replace e track len)
    (cond
      ((procedure? e) (interp-format-function e track len))
      ((or (string? e) (symbol? e)) e)
      ((pair? e) (interp-format-list e track len))
      (else (assert #f "format-replace" e))))

  (: interp-format-function ((track fixnum -> *) track fixnum -> string))
  (define (interp-format-function fun track len)
    (handle-exceptions e
      (begin (scmus-error e) "<error>")
      (format "~a" (fun track len))))

  (: interp-format-list (pair track fixnum -> string))
  (define (interp-format-list l track len)
    (let loop ((rest l) (pad-right #f) (pad-char #\space) (rel #f) (width len))
      (define (calc-width)
        (if rel (integer-scale len width) width))
      (define (finish str)
        (string-stretch str pad-char (calc-width) pad-right))
      (cond
        ((not (pair? rest))
          (finish (format-replace rest track (calc-width))))
        ((eqv? (car rest) 'group)
          (finish (scmus-format (cdr rest) (calc-width) track)))
        ((eqv? (car rest) 'pad-right)
          (loop (cdr rest) #t pad-char rel width))
        ((eqv? (car rest) 'pad-zero)
          (loop (cdr rest) pad-right #\0 rel width))
        ((eqv? (car rest) 'relative)
          (loop (cdr rest) pad-right pad-char #t width))
        ((number? (car rest))
          (loop (cdr rest) pad-right pad-char rel (car rest))))))

  (: clean-nr (string -> string))
  (define (clean-nr str)
    (let ((i (string-index str #\/)))
      (if i (string-take str i) str)))

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
    ; FIXME: this function doesn't exist
    ;(track-comment track)
    "")
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
  (define (format-bitrate track len)
    (number->string (scmus-bitrate)))

  (: parse-format-spec (format-spec -> *))
  (define (parse-format-spec spec)
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
      ((#\=) 'align-right)
      ((#\^) 'align-center)
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
      ((#\<) (parse-color-spec (cdr spec)))
      ((#\() (parse-group-spec (cdr spec)))
      ((#\-) (cons 'pad-right (parse-format-spec (cdr spec))))
      ((#\0) (cons 'pad-zero (parse-format-spec (cdr spec))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        (parse-numbered-spec spec))))

  (: take-until->symbol ((list-of char) char -> symbol))
  (define (take-until->symbol chars delim)
    (string->symbol
      (list->string
        (take-while
          (lambda (x) (not (char=? x delim)))
          chars))))

  ;; Splits a parenthesized format spec in two: the part inside the parentheses
  ;; and the rest.  Handles nested specifiers and escapes.
  (: parend-split (* char char -> *))
  (define (parend-split spec open close)
    (let loop ((rest spec) (group '()) (count 0) (escaping? #f))
      (cond
        ((null? rest) (values (reverse group) #f))
        (escaping? (loop (cdr rest) (cons (car rest) group) count #f))
        (else
          (cond
            ((char=? (car rest) open)
              (loop (cdr rest) (cons (car rest) group) (+ count 1) #f))
            ((char=? (car rest) close)
              (if (> count 0)
                (loop (cdr rest) (cons (car rest) group) (- count 1) #f)
                (values (reverse group) (cdr rest))))
            ((char=? (car rest) #\\ )
              (loop (cdr rest) group count #t))
            (else
              (loop (cdr rest) (cons (car rest) group) count #f)))))))

  (: parend->symbol (* char char -> symbol))
  (define (parend->symbol spec open close)
    (string->symbol (list->string (parend-split spec open close))))

  (: parend->string (* char char -> string))
  (define (parend->string spec open close)
    (list->string (parend-split spec open close)))

  (: parse-braced-spec (format-spec -> *))
  (define (parse-braced-spec spec)
    (let ((meta (parend->symbol spec #\{ #\})))
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
      ((bitrate)      format-bitrate)
      ((host)         (lambda (track len) (scmus-hostname)))
      ((port)         (lambda (track len) (scmus-port)))
      (else           (lambda (track len) (track-meta track meta))))))

  (: parse-code-spec (format-spec -> *))
  (define (parse-code-spec spec)
    (let* ((str (list->string (parend-split spec #\[ #\])))
           (obj (user-eval-string str)))
      (if (procedure? obj)
        obj
        (lambda (track len) obj))))

  (: parse-color-spec (format-spec -> *))
  (define (parse-color-spec spec)
    (let ((str (parend->string spec #\< #\>)))
      (cond
        ; both fg and bg given
        ((irregex-match "([^:]+):([^:]+)" str) =>
          (lambda (m) (string (fg-color->char (*->color-code (irregex-match-substring m 1)))
                              (bg-color->char (*->color-code (irregex-match-substring m 2))))))
        ; only fg given
        ((irregex-match "([^:]+):?" str) =>
          (lambda (m) (string (fg-color->char (*->color-code (irregex-match-substring m 1))))))
        ; only bg given
        ((irregex-match ":([^:]+)" str) =>
          (lambda (m) (string (bg-color->char (*->color-code (irregex-match-substring m 1))))))
        (else (string-append "~<" str ">")))))

  (: parse-group-spec (format-spec -> *))
  (define (parse-group-spec spec)
    (let ((group (parend-split spec #\( #\))))
      (cons 'group (*process-format group))))

  (: parse-numbered-spec (format-spec -> *))
  (define (parse-numbered-spec spec)
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
  (: format-next (format-spec -> *))
  (define (format-next spec)
    (cond
      ((char=? (car spec) #\{) (braced-next (cdr spec)))
      ((char=? (car spec) #\[) (code-next   (cdr spec)))
      ((char=? (car spec) #\<) (color-next (cdr spec)))
      ((char=? (car spec) #\-) (format-next (cdr spec)))
      ((char=? (car spec) #\() (group-next  (cdr spec)))
      ((char-numeric? (car spec)) (numbered-next spec))
      (else (cdr spec))))

  (: braced-next (format-spec -> *))
  (define (braced-next spec)
    (nth-value 1 (parend-split spec #\{ #\})))

  (: code-next (format-spec -> *))
  (define (code-next spec)
    (nth-value 1 (parend-split spec #\[ #\])))

  (: color-next (format-spec -> *))
  (define (color-next spec)
    (nth-value 1 (parend-split spec #\< #\>)))

  (: group-next (format-spec -> *))
  (define (group-next spec)
    (nth-value 1 (parend-split spec #\( #\))))

  (: skip-number (format-spec -> *))
  (define (skip-number spec)
    (cond
      ((char-numeric? (car spec)) (skip-number (cdr spec)))
      ((char=? (car spec) #\%) (cdr spec))
      (else spec)))

  (: numbered-next (format-spec -> *))
  (define (numbered-next spec)
    (let ((rest (skip-number spec)))
      (if (char=? (car rest) #\%)
        (format-next (cdr rest))
        (format-next rest))))

  (: format-spec-valid? (format-spec -> boolean))
  (define (format-spec-valid? spec)
    (if (null? spec)
      #f
      (case (car spec)
        ((#\a #\A #\l #\D #\n #\t #\g #\c #\y #\d #\f #\F #\~ #\= #\^ #\P #\p #\T
          #\v #\R #\r #\S #\C) #t)
        ((#\{) (braced-spec-valid? (cdr spec)))
        ((#\[) (code-spec-valid?   (cdr spec)))
        ((#\<) (color-spec-valid? (cdr spec)))
        ((#\() (group-spec-valid?  (cdr spec)))
        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) (numbered-spec-valid? spec))
        ((#\-) (format-spec-valid? (cdr spec)))
        (else #f))))

  (: braced-spec-valid? (format-spec -> boolean))
  (define (braced-spec-valid? spec)
    (nth-value 1 (parend-split spec #\{ #\})))

  (: code-spec-valid? (format-spec -> boolean))
  (define (code-spec-valid? spec)
    (let-values (((code rest) (parend-split spec #\[ #\])))
      (handle-exceptions e
        (begin (scmus-error e) #f)
        (read (open-input-string (list->string code)))
        rest)))

  (: color-spec-valid? (format-spec -> boolean))
  (define (color-spec-valid? spec)
    (let-values (((str rest) (parend-split spec #\< #\>)))
      ;(*->color-code (list->string color))
      ; FIXME
      #t))

  (: group-spec-valid? (format-spec -> boolean))
  (define (group-spec-valid? spec)
    (let-values (((group rest) (parend-split spec #\( #\))))
      (and rest (*format-string-valid? group))))

  (: numbered-spec-valid? (format-spec -> *))
  (define (numbered-spec-valid? spec)
    (format-spec-valid? (skip-number spec)))

  ;; this should be called on any user-entered format string
  (: format-string-valid? (string -> boolean))
  (define (format-string-valid? str)
    (*format-string-valid? (string->list str)))

  (: *format-string-valid ((list-of char) -> boolean))
  (define (*format-string-valid? chars)
    (cond
      ((null? chars) #t)
      ((not (char=? (car chars) #\~))
        (*format-string-valid? (cdr chars)))
      ((format-spec-valid? (cdr chars))
        (*format-string-valid? (format-next (cdr chars))))
      (else #f)))

  ;; replaces format specifiers with symbols.
  ;; str is assumed valid.
  (: process-format (string -> format-spec))
  (define (process-format str)
    (*process-format (string->list str)))

  (: *process-format ((list-of char) -> format-spec))
  (define (*process-format chars)
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
    (stringify-format (parse-format chars))))
