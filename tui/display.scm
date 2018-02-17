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

(module scmus.tui.display *
  (import coops)
  (import drewt.ncurses)
  (import scmus.base)

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
  (define *colors* (make-vector NR-CURSED))

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

  (: cursed-set! (fixnum -> fixnum))
  (define (cursed-set! cursed)
    (bkgdset (bitwise-ior (COLOR_PAIR cursed)
                          (cursed-attr cursed)))
    cursed)

  (: find-pair (fixnum fixnum -> (or fixnum boolean)))
  (define (find-pair fg bg)
    (let loop ((i 1))
      (if (< i (COLOR_PAIRS))
        (let-values (((p-fg p-bg) (pair_content i)))
          (if (and (= p-fg fg)
                   (= p-bg bg))
            i
            (loop (+ i 1))))
        #f)))

  (: cursed-temp-set! (fixnum #!optional fixnum fixnum fixnum -> undefined))
  (define cursed-temp-set!
    (let ((next (+ NR-CURSED 1)))
      (define (cursed-set! pair attr)
        (bkgdset (bitwise-ior (COLOR_PAIR pair) attr)))
      (lambda (base #!optional (fg (cursed-fg base))
                               (bg (cursed-bg base))
                               (attr (cursed-attr base)))
        (cond
          ((or (>= fg (COLORS)) (>= bg (COLORS))) (void))
          ((find-pair fg bg) => (lambda (x) (cursed-set! x attr)))
          (else
            (let ((this next))
              (set! next (if (< next (- (COLOR_PAIRS) 1))
                           (+ next 1)
                           (+ NR-CURSED 1)))
              (init_pair this fg bg)
              (cursed-set! this attr)))))))

  ;; colors }}}

  ; TODO: instead of embedding color codes into string, just use a list with strings and color
  ;       codes... this is more efficient anyway since we don't have to scan the string before
  ;       printing
  (: format-addstr! (string fixnum -> fixnum))
  (define (format-addstr! str cursed)
    (let loop ((str str))
      (let ((i (string-index str color-code?)))
        (if i
          (let ((code (ch->color-code (string-ref str i))))
            (addstr (string-take str i))
            ;(cursed-aux-set! code)
            (if (= code -2)
              (cursed-set! cursed)
              (cursed-temp-set! cursed code))
            (loop (substring/shared str (+ i 1))))
          (addstr str))))
    (string-width str))

  (: print-line! (string fixnum fixnum fixnum fixnum -> undefined))
  (define (print-line! str col line nr-cols cursed)
    (move line col)
    (cursed-set! cursed)
    (let ((written (format-addstr! (string-truncate str nr-cols) cursed)))
      (when (< written nr-cols)
        (addstr (make-string (- nr-cols written) #\space))))))
