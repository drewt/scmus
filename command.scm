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

(declare (unit command)
         (uses command-line error event scmus-eval))

;;
;; This module provides an interpreter for a simple command language.  The
;; primary motivation for this interpreter is to provide a complete interface to
;; scmus without requiring that the user learn Scheme.
;;
;; The syntax of the command language is similar to POSIX shell.  The first
;; word of the command is the command name, which resolves to a program
;; (defined as a Scheme procedure).  The remaining words are passed as
;; arguments to the program.  E.g.
;;
;;     echo Hello, world!
;;
;; The above command runs the "echo" program with the arguments "Hello," and
;; "world!".
;;
;; Multi-word arguments may be provided to a program by escaping the whitespace
;; separating the words.  This can be done by adding a backslash before the
;; whitespace character, or by wrapping the whole argument in single- or
;; double-quotation marks.  E.g.
;;
;;     echo Hello,\ world!
;;     echo 'Hello, world!'
;;     echo "Hello, world!"
;;
;; Inside of double-quotation marks, whitespace is implicitly escaped.
;; Inside of single-quotation marks, all characters are implicitly escaped.
;; As such, it is not possible to use escape codes or evaluate scheme code
;; contained within single-quotation marks.
;;
;; Commands may execute Scheme code using the '$' character.  E.g.
;;
;;     echo Hello, $my-name !
;;     echo "Hello, ${my-name}!"
;;
;; When the interpreter encounters an unescaped '$' character, it reads a
;; Scheme expression from the command string, evaluates it, and replaces the
;; expression with the result formatted as if by the DISPLAY function.
;;
(module scmus.command (define-command
                       register-command!
                       run-command)
  (import ports srfi-69)
  (import scmus.base scmus.command-line scmus.error scmus.eval scmus.event)

  (define *commands* (make-hash-table test: string=?
                                      hash: string-hash))

  (define (register-command! name handler)
    (hash-table-set! *commands* name handler))

  (define-syntax define-command
    (syntax-rules ()
      ((define-command (name . args) first . rest)
        (register-command! name (lambda args first . rest)))))

  (define (read-word)
    (define (read-datum)
      (reverse (string->list (format #f "~a" (user-eval/raw (read))))))
    ; consume leading whitespace chars
    (let loop ()
      (let ((c (peek-char)))
        (when (and (not (eof-object? c))
                   (char-whitespace? c))
          (read-char)
          (loop))))
    ; read word chars
    (let loop ((result '()) (escaping #f) (quoting #f))
      (let ((c (read-char)))
        (cond
          ((eof-object? c)
            (if (null? result)
              c ; #!eof
              (list->string (reverse result))))
          (escaping
            ; escape codes?
            (loop (cons c result) #f quoting))
          ((char=? c #\')
            (cond ((eqv? quoting 'double) (loop (cons c result) #f quoting))
                  ((eqv? quoting 'single) (loop result #f #f))
                  (else                   (loop result #f 'single))))
          ((char=? c #\")
            (cond ((eqv? quoting 'single) (loop (cons c result) #f quoting))
                  ((eqv? quoting 'double) (loop result #f #f))
                  (else                   (loop result #f 'double))))
          ((eqv? quoting 'single)
            (loop (cons c result) #f quoting))
          ((char=? c #\\)
            (loop result #t quoting))
          ((char=? c #\$)
            (let ((chars (if (char=? (peek-char) #\{)
                           (let ((chars (begin (read-char) (read-datum))))
                             (if (char=? (peek-char) #\})
                               (read-char)
                               ; TODO: raise error
                               )
                             chars)
                           (read-datum))))
              (loop (append chars result) #f quoting)))
          ((and (not quoting)
                (char-whitespace? c))
            (list->string (reverse result)))
          (else
            (loop (cons c result) #f quoting))))))
  
  (define (read-command)
    (let loop ((result '()))
      (let ((word (read-word)))
        (if (eof-object? word)
          (reverse result)
          (loop (cons word result))))))
 
  (define (run-command str)
    (define (exn-message e)
      ((condition-property-accessor 'exn 'message) e))
    (define (*run-command)
      (let ((cmd (with-input-from-string str read-command)))
        (when (not (null? cmd))
          (let ((fun (hash-table-ref/default *commands* (car cmd) #f)))
            (if fun
              (apply fun (cdr cmd))
              (command-line-print-error! (format "Unknown command: ~s" (car cmd))))))))
    (condition-case (*run-command)
      (e (exn syntax)
        (command-line-print-error! (format "Read error: ~a" (exn-message e))))
      (e (exn sandbox)
        (command-line-print-error! (format "Error during eval: ~a" (exn-message e))))
      (e (exn)
        (command-line-print-error! (format "Error: ~a" (exn-message e)))))))
