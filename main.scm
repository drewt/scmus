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

(declare (uses ui-curses
               config))

(define *version-text* "scmus 0.1\nCopyright (C) 2014 Drew Thoreson\n")
(define *help-text* "I'll write docs later, OK?\n")

(define *error* #f)

(foreign-declare "#include <locale.h>")
(define set-locale
  (foreign-lambda* c-string ((integer category) (c-string locale))
                   "return(setlocale(category, locale));"))

(define (main return)
  (define (*main)
    (scmus-update-status!)
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
    (verbose-printf "Connecting to ~a:~a...~n" *mpd-address* *mpd-port*)
    (init-client *mpd-address* *mpd-port*)
    (verbose-printf "Initializing environment...~n")
    (init-sandbox)
    (handle-exceptions x
      (printf "WARNING: failed to load ~a~n" *sysrc-path*)
      (user-load *sysrc-path*))
    (handle-exceptions x
      (void)
      (user-load *scmusrc-path*))
    (verbose-printf "Initializing ncurses...~n")
    (init-curses)
    (set-input-mode! 'normal-mode)))

(define (exit-all)
  (exit-curses)
  (exit-client))

(define (process-args args)
  (define (port-valid? port)
    (and (number? port) (> port 0) (< port 65536)))
  (when (not (null? args))
    (case (string->symbol (car args))
      ((--verbose)
       (set! *verbose* #t))
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

(process-args (command-line-arguments))
(init-all)
(let ((code (call/cc main)))
  (exit-all)
  (when *error*
    (print "Unexpected error.  Exiting.")
    (debug-pp (condition->list *error*)))
  (exit code))
