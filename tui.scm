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
  (import drewt.ncurses
          scmus.base)
  (reexport scmus.tui.display
            scmus.tui.frame
            scmus.tui.input
            scmus.tui.misc
            scmus.tui.split-pane
            scmus.tui.widget)

  (define (init-ui root-widget)
    (initscr)
    (cbreak)
    (keypad (stdscr) #t)
    (halfdelay 5)
    (noecho)
    (when (has_colors)
      (start_color)
      (use_default_colors)))

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
        ((= rc KEY_CODE_YES)
          (if (= ch KEY_RESIZE)
            (draw-ui root-widget)
            (do-handle-input root-widget ch)))
        ((not (= rc ERR))
          (do-handle-input root-widget (integer->char ch)))))))
