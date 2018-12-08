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

(import drewt.ncurses)
(import scmus.base scmus.format scmus.option scmus.tui scmus.widgets)

(define (alist->kv-rows alist)
  (map (lambda (pair)
         `(key-value . ((key   . ,(car pair))
                        (value . ,(cdr pair)))))
       alist))

(define *key-value-format* (process-format "~-50%{key} ~{value}"))

;; Generates a function to call cursed-set! with the appropriate value given
;; a window, row, and line number.
(: win-cursed-fn (#!optional (* -> boolean) -> (window * fixnum -> fixnum)))
(define (win-cursed-fn #!optional current?)
  (lambda (window row line-nr)
    (let* ((current (if current? (current? row) #f))
           (row-pos (+ (window-top-pos window) (- line-nr 1)))
           (selected (= row-pos (window-sel-pos window)))
           (marked (member row-pos (window-marked window))))
      (cond
        ((and current selected) CURSED-WIN-CUR-SEL)
        (current                CURSED-WIN-CUR)
        (selected               CURSED-WIN-SEL)
        (marked                 CURSED-WIN-MARKED)
        (else                   CURSED-WIN)))))
