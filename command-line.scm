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
         (uses editable eval-mode ui-curses))

(require-extension ncurses)

(define (command-line-changed!)
  (register-event! 'command-line-changed))

(define (command-line-empty?)
  (= 0 (command-line-length)))

(define (command-line-clear!)
  (editable-clear! *command-line*)
  (command-line-changed!))

(define (command-line-text)
  (editable-text *command-line*))

(define (command-line-text-set! str)
  (assert (string? str))
  (editable-text-set! *command-line* str))

(define (command-line-cursor-pos)
  (+ 1 (editable-cursor-pos *command-line*)))

(define (command-line-length)
  (editable-length *command-line*))

(define (command-line-leave editable)
  (editable-clear! editable)
  (set! *command-line-mode* 'normal-mode)
  (set-input-mode! 'normal-mode))

(define (command-line-commit! editable)
  (let ((cmdline (editable-text editable))
        (mode *command-line-mode*))
    (command-line-leave editable)
    (if (eqv? mode 'eval-mode)
      (user-eval cmdline)
      (win-search! cmdline))))

(define (command-line-char editable ch)
  (case ch
    ((#\newline)
      (command-line-commit! editable))
    ((#\esc)
      (command-line-leave editable))
    ((#\backspace)
      (if (= 0 (editable-length editable))
        (command-line-leave editable)
        (editable-default-char-handler editable ch)))
    (else
      (editable-default-char-handler editable ch)))
  (command-line-changed!))

(define (command-line-key editable key)
  (cond
    ((= key KEY_ENTER)
      (command-line-commit! editable))
    ((= key KEY_UP) (void)) ; TODO: history
    ((= key KEY_DOWN) (void))
    ((= key KEY_BACKSPACE)
      (if (= 0 (editable-length editable))
        (command-line-leave editable)
        (editable-default-key-handler editable key)))
    (else (editable-default-key-handler editable key)))
  (command-line-changed!))

(define *command-line* (make-editable ""
                                      command-line-char
                                      command-line-key
                                      editable-clear!))

(define *command-line-mode* 'normal-mode)

(define (command-line-pos)
  (cons (- (LINES) 1) 1))

(define (enter-eval-mode)
  (set! *command-line-mode* 'eval-mode)
  (set-input-mode! 'edit-mode *command-line* (command-line-pos))
  (command-line-changed!))

(define (enter-search-mode)
  (set! *command-line-mode* 'search-mode)
  (set-input-mode! 'edit-mode *command-line* (command-line-pos))
  (command-line-changed!))

(define (command-line-mode) *command-line-mode*)
