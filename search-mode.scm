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
         (uses ui-curses command-line)
         (export enter-search-mode leave-search-mode search-mode-char
                 search-mode-key))

(require-extension ncurses)

(define (enter-search-mode)
  (command-line-clear!)
  (cursor-on))

(define (leave-search-mode)
  (command-line-clear!)
  (set-input-mode! 'normal-mode))

(define (search-mode-char ch)
  (case ch
    ((#\newline)
      (win-search! (command-line-text))
      (leave-search-mode))
    ((#\esc)
      (leave-search-mode))
    ((#\backspace)
      (if (command-line-empty?)
        (leave-search-mode)
        (command-line-char ch)))
    (else
      (command-line-char ch))))

(define (search-mode-key key)
  (cond
    ((= key KEY_UP) (void))
    ((= key KEY_DOWN) (void))
    ((= key KEY_BACKSPACE)
      (if (command-line-empty?)
        (leave-search-mode)
        (command-line-key key)))
    (else (command-line-key key))))
