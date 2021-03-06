;;
;; Copyright 2014-2020 Drew Thoreson
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

(module (scmus tui display)
    (current-cursed
     color->number
     safe-color->number
     attr->number
     call-with-cursed
     with-cursed
     palette-set!
     can-set-color?
     set-color
     clear-screen
     format-addstr!
     print-line!)
  (import (chicken module))
  (reexport (only (drewt ncurses)
                  COLOR_BLACK
                  COLOR_RED
                  COLOR_GREEN
                  COLOR_YELLOW
                  COLOR_BLUE
                  COLOR_MAGENTA
                  COLOR_CYAN
                  COLOR_WHITE
                  A_NORMAL
                  A_UNDERLINE
                  A_REVERSE
                  A_BLINK
                  A_BOLD
                  A_DIM
                  A_ALTCHARSET
                  A_INVIS
                  A_ATTRIBUTES
                  A_CHARTEXT
                  A_COLOR
                  A_STANDOUT
                  A_PROTECT
                  A_LEFT
                  A_RIGHT
                  A_LOW
                  A_TOP
                  A_VERTICAL))
  (import (only (chicken fixnum) fxand fxshr)
          coops
          drewt.ncurses
          drewt.ustring
          scmus.base)

  (define current-cursed (make-parameter 1))

  ;; colors {{{

  (: color->number (* -> (or boolean fixnum)))
  (define (color->number color)
    (if (and (integer? color) (>= color -1) (< color 256))
      color
      (case color
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
        (else            #f))))

  (: safe-color->number (* -> fixnum))
  (define (safe-color->number color)
    (let ((n (color->number color)))
      (if (and n (< n (COLORS)))
        n
        -1)))

  (: attr->number (* -> (or boolean fixnum)))
  (define (attr->number attr)
    (if (integer? attr)
      attr
      (case attr
        ((default)    0)
        ((normal)     A_NORMAL)
        ((underline)  A_UNDERLINE)
        ((reverse)    A_REVERSE)
        ((blink)      A_BLINK)
        ((bold)       A_BOLD)
        ((dim)        A_DIM)
        ((altcharset) A_ALTCHARSET)
        ((invis)      A_INVIS)
        ((attributes) A_ATTRIBUTES)
        ((chartext)   A_CHARTEXT)
        ((color)      A_COLOR)
        ((standout)   A_STANDOUT)
        ((protect)    A_PROTECT)
        ((left)       A_LEFT)
        ((right)      A_RIGHT)
        ((low)        A_LOW)
        ((top)        A_TOP)
        ((vertical)   A_VERTICAL)
        (else         #f))))

  (: *colors* vector)
  (define *colors* (make-vector 255 #f))

  (define (alloc-cursed)
    (let loop ((i 0))
      (if (< i 255)
        (if (vector-ref *colors* i)
          (loop (+ i 1))
          (+ i 1))
        ; FIXME: should throw an error here
        255)))

  (define (init-cursed! cursed attrs)
    (vector-set! *colors* (cursed-i cursed) attrs)
    (init_pair cursed (third attrs) (second attrs))
    cursed)

  (: cursed-i (fixnum -> fixnum))
  (define (cursed-i cursed)
    (- cursed 1))

  (: cursed-attr (fixnum -> fixnum))
  (define (cursed-attr cursed)
    (car (vector-ref *colors* (cursed-i cursed))))

  (: cursed-bg (fixnum -> fixnum))
  (define (cursed-bg cursed)
    (cadr (vector-ref *colors* (cursed-i cursed))))

  (: cursed-fg (fixnum -> fixnum))
  (define (cursed-fg cursed)
    (caddr (vector-ref *colors* (cursed-i cursed))))

  (: cursed-set! (fixnum #!optional fixnum -> fixnum))
  (define (cursed-set! cursed #!optional (attr (cursed-attr cursed)))
    (current-cursed cursed)
    (bkgdset (bitwise-ior (COLOR_PAIR cursed) attr))
    cursed)

  ; Find the color pair number for (FG,BG), if it exists.
  ; TODO: we could keep a hash table keyed on ((FG << 8) | BG) for fast lookup
  (: find-pair (fixnum fixnum -> (or fixnum boolean)))
  (define (find-pair fg bg)
    (let loop ((i 0))
      (if (>= i 255)
        #f
        (let ((colors (vector-ref *colors* i)))
          (if (and colors
                   (= (cadr colors) bg)
                   (= (caddr colors) fg))
            (+ i 1)
            (loop (+ i 1)))))))

  (: cursed-temp-set! (fixnum fixnum fixnum -> undefined))
  (define (cursed-temp-set! fg bg attr)
    (cond
      ((or (>= fg (COLORS)) (>= bg (COLORS))) (void))
      ((find-pair fg bg) => (lambda (x) (cursed-set! x attr)))
      (else (cursed-set! (init-cursed! (alloc-cursed) (list attr bg fg)) attr))))

  (define (call-with-cursed fn cursed)
    (if cursed
      (let ((old-cursed (current-cursed)))
        (cursed-set! cursed)
        (fn)
        (cursed-set! old-cursed))
      (fn)))

  (define-syntax with-cursed
    (syntax-rules ()
      ((with-cursed cursed first . rest)
        (call-with-cursed (lambda () first . rest) cursed))))

  (define (palette-set! palette)
    (for-each (lambda (pair) (init-cursed! (car pair) (cdr pair)))
              palette))

  (: can-set-color? (-> boolean))
  (define (can-set-color?)
    (can_change_color))

  (: parse-rgb (fixnum -> fixnum fixnum fixnum))
  (define (parse-rgb n)
    (values (inexact->exact (round (* (fxshr (fxand n #xFF0000) 16) (/ 1000.0 255.0))))
            (inexact->exact (round (* (fxshr (fxand n #x00FF00) 8)  (/ 1000.0 255.0))))
            (inexact->exact (round (* (fxand n #x0000FF)            (/ 1000.0 255.0))))))

  (: set-color (fixnum fixnum #!optional fixnum fixnum -> undefined))
  (define (set-color color r-or-rgb #!optional g b)
    ; TODO: allow symbol for color argument
    (when (can_change_color)
      (if b
        (init_color color r-or-rgb g b)
        (let-values (((r g b) (parse-rgb r-or-rgb)))
          (init_color color r g b)))))

  ;; colors }}}

  ;; This is a really stupid macro which is unfortunately required because
  ;; curses returns ERR when writing to the bottom-rightmost cell, which
  ;; causes an exception to be thrown.  So whenever we're writing to an
  ;; arbitrary screen location, we have to set up an exception handler to
  ;; catch and ignore this particular "error" condition.
  ;; FIXME: it should be possible to continue executing on spurious errors
  (define-syntax safe-curses
    (syntax-rules ()
      ((safe-curses first rest ...)
        (handle-exceptions exn
                           (let-values (((y x) (getyx (stdscr))))
                             (unless (and (= y (- (LINES) 1))
                                          (= x (- (COLS) 1)))
                               (raise exn)))
          first rest ...))))

  (define (clear-screen x y cols rows)
    (safe-curses
      (let clear-line ((line y))
        (when (< (- line y) rows)
          (move line x)
          (let clear-cells ((cell x))
            (when (< (- cell x) cols)
              (addch #\space)
              (clear-cells (+ cell 1))))
          (clear-line (+ line 1))))))

  (define (safe-addstr str)
    (safe-curses (addstr str)))

  ; TODO: instead of embedding color codes into string, just use a list with strings and color
  ;       codes... this is more efficient anyway since we don't have to scan the string before
  ;       printing
  (: format-addstr! (string fixnum fixnum -> undefined))
  (define (format-addstr! str col row)
    (move row col)
    (let ((old-cursed (current-cursed)))
      (let loop ((str str))
        (let ((this-cursed (current-cursed))
              (i (string-index str color-code?)))
          (if i
            (let-values (((code fg-or-bg) (ch->color-code (string-ref str i))))
              (safe-addstr (string-take str i))
              (if (= code -2)
                (cursed-set! old-cursed)
                (if (eqv? fg-or-bg 'fg)
                  (cursed-temp-set! code (cursed-bg this-cursed) (cursed-attr this-cursed))
                  (cursed-temp-set! (cursed-fg this-cursed) code (cursed-attr this-cursed))))
              (loop (substring/shared str (+ i 1))))
            (safe-addstr str)))))
    (void))

  (: print-line! (string fixnum fixnum fixnum -> undefined))
  (define (print-line! str col line nr-cols #!optional (fill #\space))
    (format-addstr! (ustring-stretch str fill nr-cols #t) col line)))
