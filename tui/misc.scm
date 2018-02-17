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

(module scmus.tui.misc *
  (import coops
          drewt.ncurses
          scmus.base
          scmus.tui.widget)

  (define-class <separator> (<widget>)
    ((char initform: #\space
           accessor: separator-char)))

(define-method (print-widget! (separator <separator>) x y cols rows)
  (let loop ((row y))
    (when (< (- row y) rows)
      (move row x)
      (let loop ((col 0))
        (when (< col cols)
          (addch (separator-char separator))
          (loop (+ col 1))))
      (loop (+ row 1))))))
