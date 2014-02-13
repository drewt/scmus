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

(require-extension ncurses)

(declare (uses scmus-client))

(include "config.scm")

(define *version-text* "scmus 0.1\nCopyright (C) 2014 Drew Thoreson\n")
(define *help-text* "I'll write docs later, OK?\n")

;; TODO: put this somewhere appropriate
(define-syntax catch
  (syntax-rules ()
    ((catch body handler)
     (call-with-current-continuation
       (lambda (k)
         (with-exception-handler
           (lambda (x) (k (handler x)))
           (lambda () body)))))))

;; test repl
(define (repl n)
  (printf "#;~a> " n)
  (let ((read-val (read)))
    (if (eqv? read-val #!eof)
      (newline)
      (begin
        (display (eval read-val))
        (newline)
        (repl (+ n 1))))))

(define (main)
  ;(update)
  ;(main)
  (repl 0)
  #f
  )

(define (start-color)
  #f
  )

(define (update-colors)
  #f
  )

(define (init-curses)
  (initscr)
  (cbreak)
  (keypad (stdscr) #t)
  (halfdelay 5)
  (noecho)
  (start-color)
  (update-colors))

(define (init-all)
  (catch
    (begin
      (init-client *mpd-address* *mpd-port*)
      ;(init-curses)
      )
    (lambda (x)
      (print "Failed to initialize scmus.  Exiting.")
      (exit-all 1))))

(define (exit-all code)
  ;(endwin)
  (exit code))

(define (process-args args)
  (define (port-valid? port)
    (and (number? port) (> port 0) (< port 65536)))
  (when (not (null? args))
    (case (string->symbol (car args))
      ((-v --version)
       (begin
         (display *version-text*)
         (exit 0)))
      ((-h --help)
       (begin
         (display *help-text*)
         (exit 0)))
      ((-a --address)
       (begin
         (set! *mpd-address* (cadr args))
         (set! args (cdr args))))
      ((-p --port)
       (let ((port (string->number (cadr args))))
         (when (not (port-valid? port))
           (printf "Invalid port: ~a~n" (cadr args))
           (exit 1))
         (set! *mpd-port* port)
         (set! args (cdr args))))
      (else
        (printf "Unrecognized option: ~a~n" (car args))))
    (process-args (cdr args))))

(load *scmusrc-path*)
(process-args (command-line-arguments))
(init-all)
(main)
(exit-all 0)
