;;
;; Copyright 2014-2020 Drew Thoreson
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
(module (scmus ueval)
    (user-doc-ref
     user-eval
     user-eval/raw
     user-eval-stop
     user-eval-string
     user-export!
     user-load
     user-macro-set!
     user-value-ref
     *user-value-ref
     user-value-set!)
  (import (chicken port)
          (srfi 69)
          sandbox)
  (import (scmus base)
          (scmus error)
          (scmus event)
          (scmus option))

  (define *user-env* (make-safe-environment parent: default-safe-environment
                                            mutable: #t))

  (define (user-macro-set! name macro)
    (safe-environment-macro-set! *user-env* name macro))

  (define (user-export! name obj)
    (safe-environment-set! *user-env* name obj))

  (define *user-api* (make-hash-table test: eqv?
                                      hash: symbol-hash))

  (: user-value-set! (symbol * string -> undefined))
  (define (user-value-set! name value doc)
    (hash-table-set! *user-api* name (cons value doc))
    (user-export! name value))

  (: user-value-ref (symbol -> *))
  (define (user-value-ref name #!optional default)
    (car (hash-table-ref/default *user-api* name (cons default 0))))

  (: *user-value-ref (symbol -> *))
  (define (*user-value-ref name #!optional default)
    (safe-environment-ref *user-env* name default))

  (: user-doc-ref (symbol -> string))
  (define (user-doc-ref name #!optional default)
    (cdr (hash-table-ref/default *user-api* name (cons 0 default))))

  ;; Halt evalation.  Meant to be used in combination with call/cc,
  ;; to implement blocking calls.
  (: user-eval-stop (-> undefined))
  (define (user-eval-stop)
    (raise (make-composite-condition
             (make-property-condition 'ueval)
             (make-property-condition 'stop))))

  (: user-eval/raw (* -> *))
  (define (user-eval/raw expr)
    (condition-case (safe-eval expr environment: *user-env*)
      (e (ueval stop) (void))))

  (: user-eval (* -> *))
  (define (user-eval expr)
    (condition-case (user-eval/raw expr)
      (e () (scmus-error e) e)))

  (: user-eval-string (string -> *))
  (define (user-eval-string str)
    (with-input-from-string str
      (lambda ()
        (handle-exceptions e (begin (scmus-error e) e)
          (let loop ((last (void)))
            (let ((input (read)))
              (if (eof-object? input)
                last
                (loop (user-eval/raw input)))))))))

  (: user-load (string -> *))
  (define (user-load path)
    (call-with-input-file path
      (lambda (in)
        (let loop ()
         (let ((input (read in)))
           (unless (eqv? input #!eof)
             (condition-case (safe-eval input environment: *user-env*)
               (e () (scmus-error e)))
             (loop))))))))
