;;
;; Copyright 2014-2017 Drew Thoreson
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

(require-extension sandbox)
 
(declare (unit eval-mode)
         (uses command-line config event format keys ncurses option
               scmus-client scmus-error ui-curses window)
         (export init-sandbox win-search! user-bind! user-eval user-eval-string
                 user-load enter-eval-mode enter-search-mode))

(import scmus-base command-line config event ncurses scmus-error)

(: enter-eval-mode thunk)
(define (enter-eval-mode)
  (command-line-get-string 'eval
    (lambda (s)
      (when s
        (let ((r (user-eval-string s)))
          (if (and (not (condition? r)) (get-option 'eval-mode-print))
            (command-line-print-info! (format "~s" r))))))))

(: enter-search-mode thunk)
(define (enter-search-mode)
  (command-line-get-string 'search
    (lambda (s)
      (when s (win-search! s)))))

(define *user-env* (make-safe-environment parent: default-safe-environment
                                          mutable: #t))

(define (user-export! name obj)
  (safe-environment-set! *user-env* name obj))

(define *user-api* '())

(: register-user-value! (symbol * string -> undefined))
(define (register-user-value! name value doc)
  (set! *user-api* (cons (list name value doc) *user-api*)))

(include "user-api.scm")

(define (init-sandbox)
  (safe-environment-macro-set! *user-env* (string->symbol "\u03bb")
    (lambda (args)
      (cons 'lambda args)))
  (for-each (lambda (info)
              (user-export! (car info) (cadr info)))
            *user-api*))

(: user-eval procedure)
(define (user-eval expr)
  (condition-case (safe-eval expr environment: *user-env*)
    (e () (scmus-error-set! e) e)))

(: user-eval-string (string -> *))
(define (user-eval-string str)
  (condition-case (safe-eval (with-input-from-string str read)
                             environment: *user-env*)
    (e () (scmus-error-set! e) e)))

(: user-load (string -> *))
(define (user-load path)
  (call-with-input-file path
    (lambda (in)
      (let loop ()
       (let ((input (read in)))
         (unless (eqv? input #!eof)
           (condition-case (safe-eval input environment: *user-env*)
             (e () (scmus-error-set! e)))
           (loop)))))))
