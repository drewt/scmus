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

(module scmus.tui *
  (import (only ports
                make-output-port)
          (only posix
                file-write
                fileno/stdout
                fileno/stderr)
          drewt.ncurses
          scmus.base)
  (reexport scmus.tui.display
            scmus.tui.frame
            scmus.tui.input
            scmus.tui.list-box
            scmus.tui.misc
            scmus.tui.split-pane
            scmus.tui.widget)

  (define ui-initialized? (make-parameter #f))

  (define (call-without-curses thunk)
    (if (and (ui-initialized?) (not (isendwin)))
      (begin (endwin)
             (let ((r (thunk)))
               (refresh)
               r))
      (thunk)))

  (define-syntax without-curses
    (syntax-rules ()
      ((without-curses first rest ...)
        (call-without-curses (lambda () first rest ...)))))

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

  (define (init-ui root-widget #!key (enable-mouse? #t))
    (initscr)
    (cbreak)
    (keypad (stdscr) #t)
    (halfdelay 5)
    (noecho)
    (when (has_colors)
      (start_color)
      (use_default_colors))
    (when enable-mouse?
      (mousemask ALL_MOUSE_EVENTS))
    (curs_set 0)
    (ui-initialized? #t))

  (define (draw-ui root-widget)
    (print-widget! root-widget 0 0 (COLS) (LINES)))

  (define (update-ui root-widget)
    ; redraw damaged widgets
    (let-values (((y x) (getyx (stdscr))))
      (when (> (LINES) 1)
        (for-each reprint-widget! (damaged-widgets))
        (clear-damaged-widgets!))
      (move y x))
    ; handle input
    (let-values (((ch rc) (get-char)))
      (cond
        ((= rc ERR) (void))
        ((= rc KEY_CODE_YES)
          (cond
            ((= ch KEY_RESIZE)
              (widget-invalidate root-widget)
              (draw-ui root-widget))
            (else
              (do-handle-input root-widget ch))))
        ; ALT+KEY generates an #\escape followed immediately by the key
        ; so we have to differentiate between ALT chords and real escapes
        ((= ch 27)
          (nocbreak) ; leave halfdelay mode
          (nodelay (stdscr) #t)
          ; try to get another character: if we succeed, then
          ; it's an M-* chord, otherwise it's a real #\escape
          (let-values (((ch rc) (get-char)))
            (if (= rc ERR)
              (do-handle-input root-widget #\escape)
              (do-handle-input root-widget (+ ch 128))))
          (nodelay (stdscr) #f)
          (cbreak)
          (halfdelay 5))
        (else
          (do-handle-input root-widget (integer->char ch)))))))
