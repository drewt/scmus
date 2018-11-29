;;
;; Copyright 2014-2018 Drew Thoreson
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

(require-extension srfi-69)
(require-extension sandbox)
 
;; XXX: Naming a unit "eval" causes segfault -- hence ueval
(module scmus.ueval (register-user-value!
                     user-value-ref
                     user-doc-ref
                     user-eval
                     user-eval/raw
                     user-eval-string
                     user-load)
  (import ports srfi-69)
  (import sandbox)
  (import scmus.base scmus.error scmus.event scmus.option)

  (define *user-env* (make-safe-environment parent: default-safe-environment
                                            mutable: #t))
  (safe-environment-macro-set! *user-env* (string->symbol "\u03bb")
    (lambda (args)
      (cons 'lambda args)))

  (define (user-export! name obj)
    (safe-environment-set! *user-env* name obj))

  (define *user-api* (make-hash-table test: eqv?
                                      hash: symbol-hash))

  (: register-user-value! (symbol * string -> undefined))
  (define (register-user-value! name value doc)
    (hash-table-set! *user-api* name (cons value doc))
    (user-export! name value))

  (: user-value-ref (symbol -> *))
  (define (user-value-ref name)
    (car (hash-table-ref *user-api* name)))

  (: user-doc-ref (symbol -> string))
  (define (user-doc-ref name)
    (cdr (hash-table-ref *user-api* name)))

  (: user-eval/raw (* -> *))
  (define (user-eval/raw expr)
    (safe-eval expr environment: *user-env*))

  (: user-eval (* -> *))
  (define (user-eval expr)
    (condition-case (safe-eval expr environment: *user-env*)
      (e () (scmus-error-set! e) e)))

  (: user-eval-string (string -> *))
  (define (user-eval-string str)
    (condition-case (user-eval/raw (with-input-from-string str read))
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
             (loop))))))))
