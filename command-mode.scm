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

(declare (unit command-mode)
         (uses ui-curses))

(define *command-line-text* '())

(define (command-line-append-char! ch)
  (set! *command-line-text* (cons ch *command-line-text*)))

(define (command-line-clear!)
  (set! *command-line-text* '()))

(define (command-line-text)
  (list->string (reverse *command-line-text*)))
 
(define (run-command cmd)
  #f
  )

(define (enter-command-mode)
  ; TODO: put colon on command line column 0
  (command-line-clear!))

(define (command-mode-char ch)
  (case ch
    ((#\newline)
      (run-command (command-line-text))
      (command-line-clear!)
      (set-input-mode! 'normal-mode))
    ((#\esc)
      (command-line-clear!)
      (set-input-mode! 'normal-mode))
    (else
      (command-line-append-char! ch))))

(define (command-mode-key key)
  (case key
    ((KEY_UP) #f)
    ((KEY_DOWN) #f)
    ((KEY_LEFT) #f)
    ((KEY_RIGHT) #f)))
