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

(declare (unit search-mode)
         (uses ui-curses
               command-mode))

(define (do-search text)
  #f
  )

(define (enter-search-mode)
  ; TODO: put colon on command line column 0
  (command-line-clear!))

(define (search-mode-char ch)
  (case ch
    ((#\newline)
      (do-search (command-line-text))
      (command-line-clear!)
      (set-input-mode! 'normal-mode))
    ((#\esc)
      (command-line-clear!)
      (set-input-mode! 'normal-mode))
    (else
      (command-line-append-char! ch))))

(define (search-mode-key key)
  (case key
    ((KEY_UP) #f)
    ((KEY_DOWN) #f)
    ((KEY_LEFT) #f)
    ((KEY_RIGHT) #f)))
