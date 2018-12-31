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
                      compile-format-string
                      format-string-valid?)
  (import irregex
          drewt.ncurses
          drewt.ustring
          scmus.base
          scmus.error
          scmus.ueval
          scmus.status
          scmus.track)

  ;;
  ;; A compiled format string is a list of format instructions.
  ;; Each format instructions is either:
  ;;  * a function taking a track and a width, and returning a value --
  ;;    the value is spliced into the output, formatted as if by DISPLAY
  ;;  * a string -- the string in spliced into the output
  ;;  * One of the symbols ALIGN-CENTER or ALIGN-RIGHT -- the portion of
  ;;    the format string following these symbols is center/right aligned
  ;;  * a list beginning with a sequence of:
  ;;    * a number             -- determines the amount of padding
  ;;    * the symbol PAD-RIGHT -- add pading on the right instead of left
  ;;    * the symbol PAD-ZERO  -- pad with zeros instead of whitespace
  ;;    * the symbol RELATIVE  -- interpred padding amount as a percentage
  ;;                              of the available width
  ;;    * the symbol GROUP     -- calls SCMUS-FORMAT recursively on the
  ;;                              remainder of the list
  ;;    -- the last element of the list (if GROUP is not given) is padded
  ;;       accordingly
  ;;
  (define-type format-instruction (or (track fixnum -> *) string symbol pair))
  (define-type compiled-format-string (list-of format-instruction))

  ;; execution {{{

  ;; The format function.  Takes a compiled format string, a width, and a track
  ;; object and renders a string accordingly.
  (: scmus-format (compiled-format-string fixnum track -> string))
  (define (scmus-format fmt len track)
    (let-values (((left center right) (*scmus-format fmt len track)))
      (if (string=? center "")
        (let* ((right-len (min (ustring-width right) len))
               (left-len  (- len right-len)))
          (string-append (ustring-stretch left  #\space left-len #t)
                         (ustring-stretch right #\space right-len)))
        (let* ((center-len (min (ustring-width center) len))
               (center-pos (- (quotient len 2)
                              (quotient center-len 2)))
               (right-len  (- len (+ center-pos center-len)))
               (left-len   center-pos))
          (string-append (ustring-stretch left   #\space left-len #t)
                         (ustring-stretch center #\space center-len)
                         (ustring-stretch right  #\space right-len))))))

  ;; Renders a format string, producing 3 values: the left, center and right
  ;; aligned portions of the rendered output.
  (: *scmus-format (compiled-format-string fixnum track -> string string string))
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

  ;; Renders a single element of a processed format string.
  (: format-replace (format-instruction track fixnum -> (or string symbol)))
  (define (format-replace e track len)
    (cond
      ((procedure? e) (interp-format-function e track len))
      ((or (string? e) (symbol? e)) e)
      ((pair? e) (interp-format-list e track len))
      (else (assert #f "format-replace" e))))

  ;; Renders a function element of a compiled format string.
  (: interp-format-function ((track fixnum -> *) track fixnum -> string))
  (define (interp-format-function fun track len)
    (handle-exceptions e
      (begin (scmus-error e) "<error>")
      (format "~a" (fun track len))))

  ;; Renders a list element of a compiled format string.
  (: interp-format-list (pair track fixnum -> string))
  (define (interp-format-list l track len)
    (let loop ((rest l) (pad-right #f) (pad-char #\space) (rel #f) (width len))
      (define (calc-width)
        (if rel (integer-scale len width) width))
      (define (finish str)
        (ustring-stretch str pad-char (calc-width) pad-right))
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

  ;; execution }}}
  ;; compilation {{{

  ;; Remove the "total" from a track/disc number.
  ;; E.g. "1/13" -> "1"
  (: clean-nr (string -> string))
  (define (clean-nr str)
    (let ((i (string-index str #\/)))
      (if i (string-take str i) str)))

  (: scmus-state-character (symbol -> string))
  (define (scmus-state->character state)
    (assert (memv state '(play stop pause unknown)) "scmus-state->character" state)
    (case state
      ((play) ">")
      ((stop) ".")
      ((pause) "|")
      ((unknown) "?")))

  ;;
  ;; Track accessor functions.  These are spliced into the processed
  ;; format string and called at render time.
  ;;

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

  ;; Compile a format spec.  Returns two values: the value to
  ;; substitute for the specifier in the compiled format string,
  ;; and the remainder of the input after consuming the specifier.
  (: compile-format-spec ((list-of char) -> * (list-of char)))
  (define (compile-format-spec spec)
    (case (car spec)
      ((#\a) (values format-artist (cdr spec)))
      ((#\l) (values format-album (cdr spec)))
      ((#\A) (values format-albumartist (cdr spec)))
      ((#\D) (values format-discnumber (cdr spec)))
      ((#\n) (values format-tracknumber (cdr spec)))
      ((#\t) (values format-title (cdr spec)))
      ((#\g) (values format-genre (cdr spec)))
      ((#\c) (values format-comment (cdr spec)))
      ((#\y) (values format-date (cdr spec)))
      ((#\d) (values format-duration (cdr spec)))
      ((#\f) (values format-path (cdr spec)))
      ((#\F) (values format-filename (cdr spec)))
      ((#\=) (values 'align-right (cdr spec)))
      ((#\^) (values 'align-center (cdr spec)))
      ((#\P) (values format-playing (cdr spec)))
      ((#\p) (values format-current (cdr spec)))
      ((#\T) (values format-db-playtime (cdr spec)))
      ((#\v) (values format-volume (cdr spec)))
      ((#\R) (values format-repeat (cdr spec)))
      ((#\r) (values format-random (cdr spec)))
      ((#\S) (values format-single (cdr spec)))
      ((#\C) (values format-consume (cdr spec)))
      ((#\{) (compile-braced-spec (cdr spec)))
      ((#\[) (compile-code-spec (cdr spec)))
      ((#\<) (compile-color-spec (cdr spec)))
      ((#\() (compile-group-spec (cdr spec)))
      ((#\-) (let-values (((next rest-spec) (compile-format-spec (cdr spec))))
               (values (cons 'pad-right next) rest-spec)))
      ((#\0) (let-values (((next rest-spec) (compile-format-spec (cdr spec))))
               (values (cons 'pad-zero next) rest-spec)))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        (compile-numbered-spec spec))
      (else (values (string #\~ (car spec)) (cdr spec)))))

  ;; Splits a parenthesized format spec in two: the part inside the parentheses
  ;; and the rest.  Handles nested specifiers and escapes.
  (: parend-split ((list-of char) char char -> (list-of char) (list-of char)))
  (define (parend-split spec open close)
    (let loop ((rest spec) (group '()) (count 0) (escaping? #f))
      (cond
        ; reached the end of input without a closing paren
        ((null? rest) (values (reverse group) #f))
        ; last char was '\': escape this char
        (escaping? (loop (cdr rest) (cons (car rest) group) count #f))
        ; open paren: increment COUNT
        ((char=? (car rest) open)
          (loop (cdr rest) (cons (car rest) group) (+ count 1) #f))
        ; close paren: decrement COUNT, or return if COUNT is zero
        ((char=? (car rest) close)
          (if (> count 0)
            (loop (cdr rest) (cons (car rest) group) (- count 1) #f)
            (values (reverse group) (cdr rest))))
        ; backslash: escape next char
        ((char=? (car rest) #\\ )
          (loop (cdr rest) group count #t))
        (else
          (loop (cdr rest) (cons (car rest) group) count #f)))))

  ;; Compile a ~{...} specifier.
  (: compile-braced-spec ((list-of char) -> * (list-of char)))
  (define (compile-braced-spec spec)
    (let*-values (((parend rest-spec) (parend-split spec #\{ #\}))
                  ((meta)             (string->symbol (list->string parend))))
      (values
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
          (else           (lambda (track len) (track-meta track meta))))
        rest-spec)))

  ;; Compile a ~[...] specifier.
  (: compile-code-spec ((list-of char) -> * (list-of char)))
  (define (compile-code-spec spec)
    (let*-values (((parend rest-spec) (parend-split spec #\[ #\]))
                  ((obj)              (user-eval-string (list->string parend))))
      (values
        (if (procedure? obj)
          obj
          (lambda (track len) obj))
        rest-spec)))

  ;; Convert an arbitrary value to an ncurses color code.
  (: *->color-code (* -> (or fixnum false)))
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

  (define color-reset-string (make-parameter (string (fg-color->char -2))))

  ;; Compile a ~<...> specifier.
  (: compile-color-spec ((list-of char) -> * (list-of char)))
  (define (compile-color-spec spec)
    (define (color-spec->string str)
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
        (else (string-append "<" str ">"))))
    (let*-values (((parend rest-spec) (parend-split spec #\< #\>))
                  ((str)              (list->string parend)))
      (values
        (cond
          ((irregex-match "<([^>]*)>(.*)" str) =>
            (lambda (m)
              (let ((color-str (color-spec->string (irregex-match-substring m 1)))
                    (rest-str (irregex-match-substring m 2)))
                `(splice ,color-str
                         ,@(parameterize ((color-reset-string color-str))
                             (%compile-format (string->list rest-str)))
                         ,(color-reset-string)))))
          (else (color-spec->string str)))
        rest-spec)))

  ;; Compile a ~(...) specifier.
  (: compile-group-spec ((list-of char) -> * (list-of char)))
  (define (compile-group-spec spec)
    (let-values (((group rest-spec) (parend-split spec #\( #\))))
      (values (cons 'group (%compile-format group))
              rest-spec)))

  ;; Compile a ~# specifier.
  (: compile-numbered-spec ((list-of char) -> * (list-of char)))
  (define (compile-numbered-spec spec)
    (define (*compile-spec spec n)
      (cond
        ((char-numeric? (car spec))
          (*compile-spec (cdr spec)
                       (+ (* n 10)
                          (- (char->integer (car spec))
                             (char->integer #\0)))))
        ((char=? (car spec) #\%)
          (let-values (((next rest-spec) (compile-format-spec (cdr spec))))
            (values (cons 'relative (cons n next))
                    rest-spec)))
        (else
          (let-values (((next rest-spec) (compile-format-spec spec)))
            (values (cons n next) rest-spec)))))
    (*compile-spec spec 0))

  ;; External interface for compiling format strings.
  (: compile-format-string (string -> compiled-format-string))
  (define (compile-format-string str)
    (%compile-format (string->list str)))

  (: %compile-format ((list-of char) -> compiled-format-string))
  (define (%compile-format chars)
    ; first pass: extract format specifiers from char list and replace them
    ;             with formatter instructions
    (define (compile-format in)
      (let loop ((in in) (out '()))
        (cond
          ((null? in)
            (reverse out))
          ((not (char=? (car in) #\~))
            (loop (cdr in) (cons (car in) out)))
          (else
            (let-values (((compiled rest) (compile-format-spec (cdr in))))
              (loop rest (cons compiled out)))))))
    ; second pass: combine consecutive chars/strings
    (define (stringify-format fmt)
      (let loop ((rest (cdr fmt))
                 (last (if (char? (car fmt)) (string (car fmt)) (car fmt)))
                 (rv '()))
        (cond
          ((null? rest) (reverse (cons last rv)))
          ((and (string? last) (char? (car rest)))
            (loop (cdr rest) (string-append last (string (car rest))) rv))
          ((and (string? last) (string? (car rest)))
            (loop (cdr rest) (string-append last (car rest)) rv))
          ((char? (car rest))
            (loop (cdr rest) (string (car rest)) (cons last rv)))
          ((and (pair? (car rest))
                (eqv? (caar rest) 'splice))
            (loop (append (cdar rest) (cdr rest)) last rv))
          (else
            (loop (cdr rest) (car rest) (cons last rv))))))
    (stringify-format (compile-format chars)))

  ;; compilation }}}
  ;; validation {{{

  ;; Validate a format specifier.
  (: format-spec-valid? ((list-of char) -> (or false (list-of char))))
  (define (format-spec-valid? spec)
    (if (null? spec)
      #f
      (case (car spec)
        ((#\a #\A #\l #\D #\n #\t #\g #\c #\y #\d #\f #\F #\~ #\= #\^ #\P #\p #\T
          #\v #\R #\r #\S #\C) (cdr spec))
        ((#\{) (braced-spec-valid? (cdr spec)))
        ((#\[) (code-spec-valid?   (cdr spec)))
        ((#\<) (color-spec-valid? (cdr spec)))
        ((#\() (group-spec-valid?  (cdr spec)))
        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) (numbered-spec-valid? spec))
        ((#\-) (format-spec-valid? (cdr spec)))
        (else #f))))

  ;; Validate a ~{...} specifier.
  (: braced-spec-valid? ((list-of char) -> (or false (list-of char))))
  (define (braced-spec-valid? spec)
    (nth-value 1 (parend-split spec #\{ #\})))

  ;; Validate a ~[...] specifier.
  (: code-spec-valid? ((list-of char) -> (or false (list-of char))))
  (define (code-spec-valid? spec)
    (let-values (((code rest) (parend-split spec #\[ #\])))
      (handle-exceptions e
        (begin (scmus-error e) #f)
        (read (open-input-string (list->string code)))
        rest)))

  ;; Validate a ~<...> specifier.
  (: color-spec-valid? ((list-of char) -> (or false (list-of char))))
  (define (color-spec-valid? spec)
    (let-values (((str rest) (parend-split spec #\< #\>)))
      ; FIXME: validate STR
      rest))

  ;; Validate a ~(...) specifier.
  (: group-spec-valid? ((list-of char) -> (or false (list-of char))))
  (define (group-spec-valid? spec)
    (let-values (((group rest) (parend-split spec #\( #\))))
      (and rest (*format-string-valid? group) rest)))

  ;; Validate a ~# specifier.
  (: numbered-spec-valid? ((list-of char) -> (or false (list-of char))))
  (define (numbered-spec-valid? spec)
    (format-spec-valid?
      (let skip-number ((spec spec))
        (cond ((char-numeric? (car spec)) (skip-number (cdr spec)))
              ((char=? (car spec) #\%)    (cdr spec))
              (else                       spec)))))

  ;; This should be called on any user-entered format string.
  (: format-string-valid? (string -> boolean))
  (define (format-string-valid? str)
    (*format-string-valid? (string->list str)))

  (: *format-string-valid ((list-of char) -> boolean))
  (define (*format-string-valid? chars)
    (cond
      ((null? chars) #t)
      ((not (char=? (car chars) #\~))
        (*format-string-valid? (cdr chars)))
      ((format-spec-valid? (cdr chars)) =>
        (lambda (rest)
          (*format-string-valid? rest)))
      (else #f))))
 ;; validation }}}
