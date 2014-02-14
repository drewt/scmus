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

(use ncurses)

(define *command-line* (make-empty-editable))
(define (command-line-clear!)
  (editable-clear! *command-line*))
(define (command-line-text)
  (editable-text *command-line*))
(define (command-line-insert! ch)
  (editable-insert! *command-line* ch))
(define (command-line-backspace!)
  (editable-backspace! *command-line*))
(define (command-line-delete-char!)
  (editable-delete-char! *command-line*))

(define (run-command cmd)
  (eval (read (open-input-string cmd))) ;; XXX: unsafe, placeholder for debug
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
    ((key= key KEY_LEFT) (editable-move-left! *command-line*))
    ((key= key KEY_RIGHT) (editable-move-right! *command-line*))
    ((key= key KEY_BACKSPACE) (command-line-backspace!))
    (else (curses-print (string-append "GOT KEY: " (number->string key)
                                       "\nBACKSPACE: " (number->string KEY_BACKSPACE))))))
