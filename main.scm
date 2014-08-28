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

(declare (uses config getopt option scmus-client ui-curses))

(foreign-declare "#include <locale.h>")

(define *error* #f)

(define *cmdline-opts*
  (list (mkopt 'verbose '("--verbose") '()
               "print some extra information to the console at run time"
               store-true)
        (mkopt 'version '("-v" "--version") '()
               "print scmus's version and exit"
               store-true)
        (mkopt 'help '("-h" "--help") '()
               "print this message and exit"
               store-true)
        (mkopt 'config '("--config") '("FILE")
               "use alternate config file"
               store-one)
        (mkopt 'address '("-a" "--address") '("ADDR")
               "IP address or hostname of the MPD server"
               store-one)
        (mkopt 'port '("-p" "--port") '("PORT")
               "port number of the MPD server"
               store-number port-valid?)
        (mkopt 'unix '("-u" "--unix") '("PATH")
               "path to UNIX domain socket"
               store-one)
        (mkopt 'password '("--password") '("PASS")
               "password for the MPD server"
               store-one)
        (mkopt 'command '("-c" "--command") '("CMD" "ARGS...")
               "send a command to the mpd server and print the response"
               store-all)))

(define (version)
  (printf "scmus ~a~n" *version*)
  (exit 0))

(define (command opts)
  (pp (apply scmus-oneshot (alist-ref 'address opts)
                           (alist-ref 'port opts)
                           (alist-ref 'password opts)
                           (alist-ref 'command opts)))
  (exit 0))

(define (option-setter name)
  (lambda (x) (set-option! name x)))

;; main loop
(define (main return)
  (set! scmus-exit return)
  (handle-exceptions exn
    (begin (set! *error* exn) 1)
    (let loop ()
      (scmus-update-client!)
      (curses-update)
      (handle-input)
      (loop))))

(define (exit-all)
  (exit-curses)
  (exit-client))

;; initialize scmus
(let ((opts (process-opts (command-line-arguments) *cmdline-opts*)))
  (if (alist-ref 'help opts) (usage *cmdline-opts* 0))
  (if (alist-ref 'version opts) (version))
  (if (alist-ref 'verbose opts) (set! *verbose* #t))
  (if (alist-ref 'command opts) (command opts))
  (if (alist-ref 'unix opts)
    (set! opts (alist-update! 'address (alist-ref 'unix opts)
                              (alist-update! 'port #f opts))))
  (handle-exceptions exn
    (begin (print "Failed to initialize scmus.  Exiting.")
           (debug-printf "~a~n" (condition->list exn))
           (exit-all)
           (exit 1))
    (set-signal-handler! signal/chld void)
    (foreign-code "setlocale(LC_CTYPE, \"\");")
    (foreign-code "setlocale(LC_COLLATE, \"\");")
    (verbose-printf "Initializing environment...~n")
    (init-sandbox)
    (verbose-printf "Loading config files...~n")
    (handle-exceptions x
      (printf "WARNING: failed to load ~a~n" *sysrc-path*)
      (user-load *sysrc-path*))
    (let ((config (alist-ref 'config opts eqv? *scmusrc-path*)))
      (handle-exceptions x
        (verbose-printf "failed to load ~a~n" config)
        (user-load config)))
    (verbose-printf "Initializing curses...~n")
    (init-curses)
    (scmus-connect! (alist-ref 'address opts)
                    (alist-ref 'port opts eqv? 'default)
                    (alist-ref 'password opts eqv? 'default))))

;; enter main loop, and clean up on exit
(let ((code (call/cc main)))
  (exit-all)
  (when *error*
    (format (current-error-port) "Unexpected error.  Exiting.~n")
    (pp (condition->list *error*) (current-error-port)))
  (exit code))
