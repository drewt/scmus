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
         (uses ui-curses
               editable))

(require-extension ncurses)

(define *command-line* (make-empty-editable))
(define (command-line-changed)
  (print-command-line (command-line-text))
  (command-line-cursor-changed))
(define (command-line-cursor-changed)
  (move (- (LINES) 1) (- (+ 1 (editable-length *command-line*))
                         (editable-pos *command-line*))))
(define (command-line-clear!)
  (editable-clear! *command-line*)
  (command-line-changed))
(define (command-line-text)
  (editable-text *command-line*))
(define (command-line-insert! ch)
  (editable-insert! *command-line* ch)
  (command-line-changed))
(define (command-line-backspace!)
  (editable-backspace! *command-line*)
  (command-line-changed))
(define (command-line-delete-char!)
  (editable-delete-char! *command-line*)
  (command-line-changed))
(define (command-line-move-left!)
  (editable-move-left! *command-line*)
  (command-line-cursor-changed))
(define (command-line-move-right!)
  (editable-move-right! *command-line*)
  (command-line-cursor-changed))

(define (run-command cmd)
  (condition-case (eval (read (open-input-string cmd)))
    (ex () #f)))

(define (enter-command-mode)
  (command-line-clear!)
  (print-command-line-char #\:)
  (cursor-on))

(define (leave-command-mode)
  (command-line-clear!)
  (print-command-line-char #\space)
  (set-input-mode! 'normal-mode))

(define (command-mode-char ch)
  (case ch
    ((#\newline)
      (run-command (command-line-text))
      (leave-command-mode))
    ((#\esc)
      (command-line-clear!)
      (set-input-mode! 'normal-mode))
    ((#\backspace)
      (command-line-backspace!))
    ((#\x4)
      (command-line-delete-char!))
    (else
      (command-line-insert! ch))))

(define (command-mode-key key)
  (cond
    ((key= key KEY_UP) #f)
    ((key= key KEY_DOWN) #f)
    ((key= key KEY_LEFT)
      (command-line-move-left!))
    ((key= key KEY_RIGHT)
      (command-line-move-right!))
    ((key= key KEY_BACKSPACE)
      (command-line-backspace!))))
