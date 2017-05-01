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
         (uses config ncurses))

(import ncurses)

;; the exit routine; initially (exit), becomes a continuation
(define scmus-exit exit)

(: *scmus-error* string)
(define *scmus-error* "")

(: *view-names* (list-of symbol))
(define *view-names*
  '(library queue search browser status error options bindings))

(define-type printf (#!rest * -> undefined))
(define-type pp (* -> undefined))

(: verbose-printf printf)
(define (verbose-printf . args)
  (if *verbose*
    (apply console-printf args)))

(: debug-printf printf)
(define (debug-printf . args)
  (if *debug*
    (apply console-printf args)))

(: debug-pp pp)
(define (debug-pp sexp)
  (if *debug*
    (console-pp sexp)))

(: console-printf printf)
(define (console-printf . args)
  (without-curses
    (apply printf args)))

(: console-pp pp)
(define (console-pp sexp)
  (without-curses
    (pp sexp)))

(: list-of (symbol list -> (list-of (pair symbol *))))
(define (list-of type lst)
  (map (lambda (x) (cons type x)) lst))

(: separator? (* -> boolean))
(define (separator? obj)
  (and (pair? obj) (eq? (car obj) 'separator)))

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

(: alist-update (* * (list-of pair) #!optional (* * -> boolean) -> (list-of pair)))
(define (alist-update key value alist #!optional (test eqv?))
  (alist-update! key value (list-copy alist) test))

(define metadata-display-names
  '((title         . "Title")
    (artist        . "Artist")
    (albumartist   . "Album Artist")
    (album         . "Album")
    (date          . "Date")
    (track         . "Track Number")
    (disc          . "Disc Number")
    (duration      . "Duration")
    (last-modified . "Last Modified")
    (file          . "File")))

(define (metadata-name key)
  (let ((name (alist-ref key metadata-display-names)))
    (if name name (symbol->string key))))

(: sort-metadata ((list-of (pair symbol *)) -> (list-of (pair string *))))
(define (sort-metadata metadata)
  ;; returns the position of key in alist, or #f.
  (define (alist-ordinal key alist)
    (let loop ((i 0) (rest alist))
      (if (null? rest)
        #f
        (if (eqv? key (caar rest))
          i
          (loop (+ i 1) (cdr rest))))))

  ;; Returns true if (car a) is before (car b) in model (an alist)
  (define (alist-compare a b model)
    (let ((ord-a (alist-ordinal (car a) model))
          (ord-b (alist-ordinal (car b) model)))
      (cond
        ((and (not ord-a) (not ord-b))
           (string<? (symbol->string (car a))
                     (symbol->string (car b))))
        ((not ord-a) #f)
        ((not ord-b) #t)
        (else (< ord-a ord-b)))))

  ;; We're doing two things here.  First, we sort the metadata using
  ;; metadata-display-names as a reference.  Then we convert the keys to
  ;; strings, using metadata-display-names to get the name strings.
  (map (lambda (x)
         (cons (metadata-name (car x))
               (cdr x)))
       (sort metadata (lambda (a b) (alist-compare a b metadata-display-names)))))

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
  (let loop ((result '()) (substr '()) (rest (string->list str)))
    (if (null? rest)
      (if (null? substr)
        (reverse result)
        (reverse (cons (list->string (reverse substr)) result)))
      (if (char=? (car rest) #\newline)
        (loop (cons (list->string (reverse substr)) result) '() (cdr rest))
        (loop result (cons (car rest) substr) (cdr rest))))))

(: seconds->string (fixnum -> string))
(define (seconds->string total-seconds)
  (assert (>= total-seconds 0) "seconds->string" total-seconds)
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

(: clean-nr (string -> string))
(define (clean-nr str)
  (let ((i (string-index str #\/)))
    (if i (string-take str i) str)))

(: error-set! (condition -> undefined))
(define (error-set! error)
  (let ((out (open-output-string)))
    (pretty-print (condition->list error) out)
    (verbose-printf "~a~n"(get-output-string out))
    (set! *scmus-error* (get-output-string out)))
  (if ((condition-predicate 'exn) error)
    (command-line-print-error!
      (format "~a: ~s" (get-condition-property error 'exn 'message)
                       (get-condition-property error 'exn 'arguments))))
  (register-event! 'error-changed))
