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

(declare (unit ui-lib)
         (uses ncurses))

;; colors {{{

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

(define (safe-color->number color)
  (let ((n (color->number color)))
    (if (< n (COLORS))
      n
      -1)))

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
 
(define *colors* (make-vector NR-CURSED))

(define (get-color-option name)
  (let ((option (get-option name)))
    (assert (list? option))
    (list (attr->number (car option))
          (safe-color->number (cadr option))
          (safe-color->number (caddr option)))))

(define (cursed-i cursed)
  (- cursed 1))

(define (cursed-attr cursed)
  (car (vector-ref *colors* (cursed-i cursed))))

(define (cursed-bg cursed)
  (cadr (vector-ref *colors* (cursed-i cursed))))

(define (cursed-fg cursed)
  (caddr (vector-ref *colors* (cursed-i cursed))))

(define (init-cursed! cursed color)
  (vector-set! *colors* (cursed-i cursed) (get-color-option color)))

(define (update-colors!)
  (define (*update-colors!)
    (let loop ((i 1))
      (when (<= i NR-CURSED)
        (init_pair i (cursed-fg i) (cursed-bg i))
        (loop (+ i 1)))))
  (init-cursed! CURSED-CMDLINE     'color-cmdline)
  (init-cursed! CURSED-ERROR       'color-error)
  (init-cursed! CURSED-INFO        'color-info)
  (init-cursed! CURSED-STATUSLINE  'color-statusline)
  (init-cursed! CURSED-TITLELINE   'color-titleline)
  (init-cursed! CURSED-WIN         'color-win)
  (init-cursed! CURSED-WIN-CUR     'color-win-cur)
  (init-cursed! CURSED-WIN-CUR-SEL 'color-win-cur-sel)
  (init-cursed! CURSED-WIN-SEL     'color-win-sel)
  (init-cursed! CURSED-WIN-MARKED  'color-win-marked)
  (init-cursed! CURSED-WIN-TITLE   'color-win-title)
  (*update-colors!)
  (cursed-set! CURSED-WIN))

(define (cursed-set! cursed)
  (bkgdset (bitwise-ior (COLOR_PAIR cursed)
                        (cursed-attr cursed)))
  cursed)

(define (find-pair fg bg)
  (let loop ((i 1))
    (if (< i (COLOR_PAIRS))
      (let-values (((p-fg p-bg) (pair_content i)))
        (if (and (= p-fg fg)
                 (= p-bg bg))
          i
          (loop (+ i 1))))
      #f)))

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
;; cursed-set! {{{
;; Generates a function to call cursed-set! with the appropriate value given
;; a window, row, and line number.
(define (win-cursed-fn current?)
  (lambda (window row line-nr)
    (let* ((current (current? row))
           (row-pos (+ (window-top-pos window) (- line-nr 1)))
           (selected (= row-pos (window-sel-pos window)))
           (marked (member row-pos (window-marked window))))
      (cursed-set!
        (cond
          ((separator? row)       CURSED-WIN-TITLE)
          ((eqv? row 'separator)  CURSED-WIN-TITLE)
          ((and current selected) CURSED-WIN-CUR-SEL)
          (current                CURSED-WIN-CUR)
          (selected               CURSED-WIN-SEL)
          (marked                 CURSED-WIN-MARKED)
          (else                   CURSED-WIN))))))

;; cursed-set! suitable for any window (CURSED-CUR[-SEL] is never chosen)
(define generic-cursed-set! (win-cursed-fn (lambda (x) #f)))
;; cursed-set! }}}
;; print-line {{{
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
        (addstr str)))))

(define (track-print-line line fmt track cursed)
  (assert (integer? line) "track-print-line" line)
  (assert (list? fmt) "track-print-line" fmt)
  (assert (list? track) "track-print-line" track)
  (mvaddch line 0 #\space)
  (format-addstr! (scmus-format fmt (- (COLS) 2) track) cursed)
  (clrtoeol))

(define (simple-print-line line-nr str)
  (mvaddstr line-nr 0
            (string-truncate (format " ~a" str)
                             (- (COLS) 2)))
  (clrtoeol))

(define (format-print-line line-nr fmt . args)
  (mvaddstr line-nr 0
            (string-truncate (apply format fmt args)
                             (- (COLS) 2)))
  (clrtoeol))

;; Generic function to print an alist entry, for use with print-window.
(define (alist-print-line window row line-nr cursed)
  (simple-print-line line-nr (car row))
  (let ((col (- (quotient (COLS) 2) 1)))
    (mvaddstr line-nr col
              (string-truncate (format " ~a" (cdr row))
                               (- (COLS) col)))))

(define (list-window-print-row window row line-nr cursed)
  (simple-print-line line-nr row))

(define (separator? row)
  (and (pair? row) (eqv? (car row) 'separator)))
;; print-line }}}
