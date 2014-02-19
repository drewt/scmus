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

(declare (unit command-line)
         (uses editable
               ui-curses))

(require-extension ncurses)

(define *command-line* (make-empty-editable))

(define (command-line-changed!)
  (ui-element-changed! 'command-line))

(define (command-line-clear!)
  (editable-clear! *command-line*)
  (command-line-changed!))

(define (command-line-text)
  (editable-text *command-line*))

(define (command-line-cursor-pos)
  (- (+ 1 (editable-length *command-line*))
     (editable-pos *command-line*)))

(define (command-line-length)
  (editable-length *command-line*))

(define (command-line-insert! ch)
  (editable-insert! *command-line* ch)
  (command-line-changed!))

(define (command-line-backspace!)
  (editable-backspace! *command-line*)
  (command-line-changed!))

(define (command-line-delete-char!)
  (editable-delete-char! *command-line*)
  (command-line-changed!))

(define (command-line-move-left!)
  (editable-move-left! *command-line*))

(define (command-line-move-right!)
  (editable-move-right! *command-line*))

(define (command-line-char ch)
  (case ch
    ((#\backspace)
      (command-line-backspace!))
    ((#\x4)
      (command-line-delete-char!))
    (else
      (command-line-insert! ch))))

(define (command-line-key key)
  (cond
    ((key= key KEY_LEFT)
      (command-line-move-left!))
    ((key= key KEY_RIGHT)
      (command-line-move-right!))
    ((key= key KEY_BACKSPACE)
      (command-line-backspace!))))
