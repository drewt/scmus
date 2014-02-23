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

(require-extension ncurses
                   srfi-13)
 
(declare (unit command-mode)
         (uses ui-curses
               command-line
               keys))

;; user functions {{{

(define (user-bind! keys context thunk)
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (make-binding! key-list context thunk)
      #f)))

(define (user-unbind! keys context)
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (unbind! key-list context)
      #f)))

;; user functions }}}

(define (run-command cmd)
  (condition-case (eval (read (open-input-string cmd)))
    (ex () (curses-print "ERROR"))))

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
    (else
      (command-line-char ch))))

(define (command-mode-key key)
  (cond
    ((key= key KEY_UP) (void))
    ((key= key KEY_DOWN) (void))
    (else (command-line-key key))))
