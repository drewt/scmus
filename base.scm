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

(module scmus.base *
  (reexport
    (except scheme
      string-length string-ref string-set! make-string string substring
      string->list list->string string-fill! write-char read-char display)
    (except chicken
      reverse-list->string print print*)
    (except data-structures
      conc string-chop string-split string-translate
      substring=? substring-ci=? substring-index substring-index-ci)
    (except extras
      read-string write-string read-token)
    srfi-1
    utf8 utf8-srfi-13 utf8-srfi-14)

  (import ports
          posix
          drewt.ncurses
          scmus.config)

  (define ui-initialized? (make-parameter #f))

  (define (call-without-curses thunk)
    (if (ui-initialized?)
      (begin (endwin)
             (let ((r (thunk)))
               (refresh)
               r))
      (thunk)))

  (define-syntax without-curses
    (syntax-rules ()
      ((without-curses first rest ...)
        (call-without-curses (lambda () first rest ...)))))
 
  ;; the exit routine; initially (exit), becomes a continuation
  (define scmus-exit exit)

  (define *console-output-port*
    (make-output-port
      (lambda (str)
        (without-curses (file-write fileno/stdout str)))
      void))

  (define *console-error-port*
    (make-output-port
      (lambda (str)
        (without-curses (file-write fileno/stderr str)))
      void))

  (current-output-port *console-output-port*)
  (current-error-port  *console-error-port*)

  (define (list-truncate in-lst n)
    (let loop ((lst in-lst) (n n) (result '()))
      (cond
        ((null? lst) in-lst)
        ((zero? n)   (reverse result))
        (else        (loop (cdr lst) (- n 1) (cons (car lst) result))))))

  (: port-valid? (* -> boolean))
  (define (port-valid? port)
    (and (integer? port) (positive? port) (< port 65536)))

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

  ;; unicode stuff {{{

  (: +unicode-private-base+ fixnum)
  (define-constant +unicode-private-base+ #xE000)

  (: color-code? (char -> boolean))
  (define (color-code? ch)
    (let ((u (char->integer ch)))
      (and (>= u +unicode-private-base+)
           (< u (+ 258 256 +unicode-private-base+)))))

  (: ch->color-code (char -> fixnum))
  (define (ch->color-code ch)
    (- (char->integer ch) +unicode-private-base+ 2))

  (: fg-color->char (fixnum -> char))
  (define (fg-color->char color)
    (integer->char (+ color +unicode-private-base+ 2)))

  (: bg-color->char (fixnum -> char))
  (define (bg-color->char color)
    (integer->char (+ color +unicode-private-base+ 258)))

  ;; substitute for broken utf8#string-contains-ci
  (: substring-match (string string -> fixnum))
  (define (substring-match str sub)
    (string-contains (string-downcase str) (string-downcase sub)))

  (: string-width (string -> fixnum))
  (define (string-width str)
    (fold + 0 (map char-width (string->list str))))

  (: char-width (char -> fixnum))
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
        ; color codes (private area)
        ((and (>= u +unicode-private-base+)
              (< u (+ 256 +unicode-private-base+))) 0)
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

  (: ustring-take (string fixnum -> string fixnum))
  (define (ustring-take str width)
    (define (finalize result space)
      (list->string
        (reverse
          (append (make-list space #\space)
                  result))))
    (let loop ((result '()) (rest (string->list str)) (r-width 0))
      (let* ((c (car rest))
             (c-width (char-width c))
             (space (- width r-width)))
        (if (< space c-width)
          (values (finalize result space) (+ r-width space))
          (loop (cons c result) (cdr rest) (+ r-width c-width))))))

  (: ustring-take-right (string fixnum -> string fixnum))
  (define (ustring-take-right str width)
    (let loop ((result '()) (rest (reverse (string->list str))) (r-width 0))
      (let* ((c (car rest))
             (c-width (char-width c)))
        (if (> (+ r-width c-width) width)
          (values (list->string result) r-width)
          (loop (cons c result) (cdr rest) (+ r-width c-width))))))

  (: string-truncate (string fixnum #!optional boolean -> string fixnum))
  (define (string-truncate s len #!optional (left #f))
    (let ((width (string-width s)))
      (if (> width len)
        (if left
          (ustring-take-right s len)
          (ustring-take s len))
        (values s width))))

  (: ustring-pad (string fixnum char #!optional boolean -> string))
  (define (ustring-pad str len c #!optional (right #f))
    (if right
      (string-append str (make-string (- len (string-width str)) c))
      (string-append (make-string (- len (string-width str)) c) str)))

  (: string-stretch (string char fixnum #!optional boolean -> string))
  (define (string-stretch str c len #!optional (right #f))
    (if (> len (string-width str))
      (values (ustring-pad str len c right) len)
      (nth-value 0 (string-truncate str len))))

  (: integer-scale (fixnum fixnum -> fixnum))
  (define (integer-scale len percent)
    (assert (>= len 0) "integer-scale" len)
    (inexact->exact (round (* len (/ percent 100)))))

  (: string-split-lines (string -> (list-of string)))
  (define (string-split-lines str)
    (let loop ((str str) (result '()))
      (let ((i (string-index str #\newline)))
        (if (not i)
          (reverse (cons str result))
          (loop (substring/shared str (+ i 1))
                (cons (substring/shared str 0 i) result)))))))
