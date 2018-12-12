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
                       load-command-script
                       register-command!
                       run-command)
  (import ports
          srfi-69
          scmus.base
          scmus.command-line
          scmus.error
          scmus.ueval
          scmus.event)

  (define *commands* (make-hash-table test: string=?
                                      hash: string-hash))

  (define (register-command! name handler)
    (hash-table-set! *commands* name handler))

  (define-syntax define-command
    (syntax-rules ()
      ((define-command (name . args) first . rest)
        (register-command! name (lambda args first . rest)))))

  ; Helper routines {{{

  ;; Read until the first non-whitespace character.
  (define (read-whitespace/all)
    (let ((c (peek-char)))
      (when (and (not (eof-object? c))
                 (char-whitespace? c))
        (read-char)
        (read-whitespace/all))))

  ;; Like READ-WHITESPACE/ALL, except that this procedure terminates before
  ;; reading a #\newline character.
  (define (read-whitespace/non-terminal)
    (let ((c (peek-char)))
      (when (and (not (eof-object? c))
                 (char-whitespace? c)
                 (not (char=? c #\newline)))
        (read-char)
        (read-whitespace/non-terminal))))

  ;; Read until #\newline or #!eof.
  (define (read-comment)
    (let ((c (peek-char)))
      (unless (or (eof-object? c)
                  (char=? c #\newline))
        (read-char)
        (read-comment))))

  ;; Read and evaluate a Scheme expression, returning the result formatted
  ;; as if by DISPLAY.
  (define (read-datum)
    (reverse (string->list (format #f "~a" (user-eval/raw (read))))))

  ; Helper routines }}}

  (define (read-word)
    (let loop ((result '()) (escaping #f) (quoting #f))
      (let ((c (read-char)))
        (cond
          ; Terminate after reaching EOF
          ((eof-object? c)
            ; TODO: warn if we're escaping or quoting
            (if (null? result)
              (values c #t) ; return #!eof
              (values (list->string (reverse result)) #t)))
          ; If the last character was a backslash, escape the current character
          (escaping
            (case c
              ((#\newline) (loop result #f quoting))
              (else        (loop (cons c result) #f quoting))))
          ; Quoting...
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
          ; Backslash: escape the next character
          ((char=? c #\\)
            (loop result #t quoting))
          ; Evaluate embedded Scheme expression
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
          ; Comment
          ((char=? c #\#)
            (read-comment)
            (loop result escaping quoting))
          ; Unquoted whitespace: terminate
          ((and (not quoting)
                (char-whitespace? c))
            (values (list->string (reverse result)) (char=? c #\newline)))
          ; Regular character
          (else
            (loop (cons c result) #f quoting))))))
  
  ;; Read a command from CURRENT-INPUT-PORT, and return it as a list of strings.
  (define (read-command)
    (read-whitespace/all)
    (let loop ((result '()))
      (read-whitespace/non-terminal)
      (let-values (((word end-of-command?) (read-word)))
        (cond
          ((eof-object? word) (reverse result))
          (end-of-command?    (reverse (cons word result)))
          (else               (loop (cons word result)))))))

  (define (eval-command cmd)
    (let ((fun (hash-table-ref/default *commands* (car cmd) #f)))
      (if fun
        (apply fun (cdr cmd))
        ; TODO: raise an exception
        (command-line-print-error! (format "Unknown command: ~s" (car cmd))))))

  (define (load-command-script file)
    (define (read-eval-loop)
      (let ((cmd (read-command)))
        (unless (null? cmd)
          (eval-command cmd)
          (read-eval-loop))))
    (with-input-from-file file read-eval-loop))
 
  (define (run-command str)
    (define (handle-error e msg)
      (scmus-error e)
      (command-line-print-error!
        (format "~a: ~a" msg ((condition-property-accessor 'exn 'message) e))))
    (condition-case (let ((cmd (with-input-from-string str read-command)))
                      (unless (null? cmd)
                        (eval-command cmd)))
      (e (exn syntax)  (handle-error e "Read error"))
      (e (exn sandbox) (handle-error e "Error during eval"))
      (e (exn)         (handle-error e "Error")))))
