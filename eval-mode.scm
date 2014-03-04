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
                   sandbox
                   srfi-13)
 
(declare (unit eval-mode)
         (uses ui-curses
               command-line
               keys)
         (export init-sandbox
                 user-load
                 enter-eval-mode
                 leave-eval-mode
                 eval-mode-char
                 eval-mode-key))

;; user functions {{{

(define (user-bind! keys context thunk #!optional (force #f))
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (begin
        (if force
          (unbind! key-list context))
        (make-binding! key-list context thunk))
      #f)))

(define (user-unbind! keys context)
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (unbind! key-list context)
      #f)))

(define (user-push! str)
  (set-input-mode! 'eval-mode)
  (command-line-text-set! str))

;; user functions }}}

(define *user-env* (make-safe-environment parent: default-safe-environment))

(define (init-sandbox)
  (safe-environment-set! *user-env* 'bind! user-bind!)
  (safe-environment-set! *user-env* 'unbind! user-unbind!)
  (safe-environment-set! *user-env* 'prev! scmus-prev!)
  (safe-environment-set! *user-env* 'play! scmus-play!)
  (safe-environment-set! *user-env* 'pause! scmus-pause!)
  (safe-environment-set! *user-env* 'stop! scmus-stop!)
  (safe-environment-set! *user-env* 'next! scmus-next!)
  (safe-environment-set! *user-env* 'seek! scmus-seek!)
  (safe-environment-set! *user-env* 'repeat-set! scmus-repeat-set!)
  (safe-environment-set! *user-env* 'random-set! scmus-random-set!)
  (safe-environment-set! *user-env* 'single-set! scmus-single-set!)
  (safe-environment-set! *user-env* 'consume-set! scmus-consume-set!)
  (safe-environment-set! *user-env* 'toggle-repeat! scmus-toggle-repeat!)
  (safe-environment-set! *user-env* 'toggle-random! scmus-toggle-random!)
  (safe-environment-set! *user-env* 'toggle-single! scmus-toggle-single!)
  (safe-environment-set! *user-env* 'toggle-consume! scmus-toggle-consume!)
  (safe-environment-set! *user-env* 'push! user-push!)
  (safe-environment-set! *user-env* 'set-view! set-view!)
  (safe-environment-set! *user-env* 'win-move! win-move!)
  (safe-environment-set! *user-env* 'win-activate! win-activate!))

(define (user-eval str)
  (assert (string? str))
  (condition-case (safe-eval (read (open-input-string str))
                             environment: *user-env*)
    (e () (curses-print "ERROR"))))

(define (user-load path)
  (assert (string? path))
  (call-with-input-file path
    (lambda (in)
      (let loop ()
       (let ((input (read in)))
         (unless (eqv? input #!eof)
           (condition-case (safe-eval input environment: *user-env*)
             (e () (void)))
           (loop)))))))

(define (enter-eval-mode)
  (command-line-clear!)
  (print-command-line-char #\:)
  (cursor-on))

(define (leave-eval-mode)
  (command-line-clear!)
  (print-command-line-char #\space)
  (set-input-mode! 'normal-mode))

(define (eval-mode-char ch)
  (assert (char? ch))
  (case ch
    ((#\newline)
      (user-eval (command-line-text))
      (leave-eval-mode))
    ((#\esc)
      (leave-eval-mode))
    ((#\backspace)
      (if (command-line-empty?)
        (leave-eval-mode)
        (command-line-char ch)))
    (else
      (command-line-char ch))))

(define (eval-mode-key key)
  (cond
    ((key= key KEY_UP) (void))
    ((key= key KEY_DOWN) (void))
    ((key= key KEY_BACKSPACE)
      (if (command-line-empty?)
        (leave-eval-mode)
        (command-line-key key)))
    (else (command-line-key key))))
