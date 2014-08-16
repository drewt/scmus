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

(declare (uses config option ui-curses))

(define *version-text* (format "scmus ~a~n" *version*))
(define *help-text* "I'll write docs later, OK?\n")

(define *error* #f)

(foreign-declare "#include <locale.h>")

(define (main return)
  (define (*main)
    (scmus-update-client!)
    (curses-update)
    (handle-input)
    (*main))
  (set! scmus-exit return)
  (handle-exceptions exn
    (begin (set! *error* exn) 1)
    (*main)))

(define (init-all)
  (handle-exceptions exn
    (begin (print "Failed to initialize scmus.  Exiting.")
           (debug-printf "~a~n" (condition->list exn))
           (exit-all)
           (exit 1))
    (foreign-code "setlocale(LC_CTYPE, \"\");")
    (foreign-code "setlocale(LC_COLLATE, \"\");")
    (scmus-connect!)
    (verbose-printf "Initializing environment...~n")
    (init-sandbox)
    (verbose-printf "Loading config files...~n")
    (handle-exceptions x
      (printf "WARNING: failed to load ~a~n" *sysrc-path*)
      (user-load *sysrc-path*))
    (handle-exceptions x
      (verbose-printf "failed to load ~a~n" *scmusrc-path*)
      (user-load *scmusrc-path*))
    (verbose-printf "Initializing curses...~n")
    (init-curses)
    (set-input-mode! 'normal-mode)))

(define (exit-all)
  (exit-curses)
  (exit-client))

(define *cmdline-opts*
  `((("--verbose") ()
     "print some extra information to the console at run time"
     ,(lambda (args)
        (set! *verbose* #t)
        args))
    (("-v" "--version") ()
     "print scmus's version and exit"
     ,(lambda (args)
        (printf "scmus ~a~n" *version*)
        (exit 0)))
    (("-h" "--help") ()
     "print this message and exit"
     ,(lambda (args)
        (print-usage)
        (exit 0)))
    (("-a" "--address") ("ADDR")
     "IP address or hostname of MPD server"
     ,(lambda (args)
        (set-option! 'mpd-address (car args))
        (cdr args)))
    (("-p" "--port") ("ADDR")
     "port number of MPD server"
     ,(lambda (args)
        (let ((port (string->number (car args))))
          (unless (and (number? port) (> port 0) (< port 65536))
            (printf "Invalid port: ~a~n" (car args))
            (exit 1))
          (set-option! 'mpd-port port)
          (cdr args))))))

(define (opt-names opt) (car opt))
(define (opt-args opt) (cadr opt))
(define (opt-doc opt) (caddr opt))
(define (opt-fun opt) (cadddr opt))

(define (print-usage)
  (print "Usage: scmus [options]")
  (print "Options:")
  (let loop ((opts *cmdline-opts*))
    (unless (null? opts)
      (display "    ")
      (for-each (lambda (x) (display x) (display #\space))
                (opt-names (car opts)))
      (for-each (lambda (x) (display x) (display #\space))
                (opt-args (car opts)))
      (newline)
      (printf "        ~a~n" (opt-doc (car opts)))
      (loop (cdr opts)))))

(define (process-opts opts)
  (define (get-opt opt)
    (let loop ((opts *cmdline-opts*))
      (cond
        ((null? opts) #f)
        ((member opt (opt-names (car opts))) (car opts))
        (else (loop (cdr opts))))))
  (unless (null? opts)
    (let ((opt (get-opt (car opts))))
      (if opt
        (process-opts ((opt-fun opt) (cdr opts)))
        (begin
          (printf "Unrecognized option: ~a~n" (car opts))
          (process-opts (cdr opts)))))))

(set-signal-handler! signal/chld void)
(process-opts (command-line-arguments))
(init-all)
(let ((code (call/cc main)))
  (exit-all)
  (when *error*
    (format (current-error-port) "Unexpected error.  Exiting.~n")
    (pp (condition->list *error*) (current-error-port)))
  (exit code))
