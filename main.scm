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

(foreign-declare "#include <locale.h>")

(import drewt.getopt
        scmus.base
        scmus.client
        scmus.config
        scmus.log
        scmus.ueval)

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

;; main loop
(define (main return)
  (set! scmus-exit return)

  (handle-exceptions exn
    (begin (set! *error* exn) 1)
    (let loop ()
      (curses-update)
      (loop))))

(define (exit-all)
  (exit-curses)
  (exit-client))

(define-syntax initialize
  (syntax-rules ()
    ((initialize system first rest ...)
       (handle-exceptions exn
         (begin (log-write! 'error (format "Initialization failed: ~a" system))
                (signal exn))
         (begin first rest ...)
         (log-write! 'info (format "Initialized ~a" system))))))

(define (read-plugins)
  (if (file-exists? *plugins-dir*)
    (map (lambda (dir)
           (cons (cons 'root dir) (read-file (string-append *plugins-dir* "/" dir "/manifest"))))
         (filter (lambda (dir) (file-exists? (string-append *plugins-dir* "/" dir "/manifest")))
                 (directory *plugins-dir*)))
    '()))

(define (load-plugins plugins)
  (for-each (lambda (plugin)
              (let ((root (string-append *plugins-dir* "/" (alist-ref 'root plugin))))
                (for-each (lambda (file)
                            (load (string-append root "/" file)))
                          (alist-ref 'load-order plugin))))
            plugins))

;; Set the terminal title
(define (set-title! title)
  (display (string-append "\x1b]2;" title "\x07"
                          (if (ui-initialized?) "\n" ""))))

(set-title! "scmus")

(define (log-filter log-type)
  (or *verbose* (memv log-type '(error warning))))

;; initialize scmus
(let ((opts (process-opts (command-line-arguments) *cmdline-opts*)))
  (if (alist-ref 'help opts) (usage *cmdline-opts* 0))
  (if (alist-ref 'version opts) (version))
  (if (alist-ref 'verbose opts) (set! *verbose* #t))
  (if (alist-ref 'command opts) (command opts))
  (if (alist-ref 'unix opts)
    (set! opts (alist-update! 'address (alist-ref 'unix opts)
                              (alist-update! 'port #f opts))))
  (current-logger (make <port-logger> 'filter log-filter
                                      'port   *console-error-port*))
  (handle-exceptions exn
    (begin (display "\nFailed to initialize scmus.  Exiting.\n")
           (exit-all)
           (exit 1))
    (initialize "signals"
      (set-signal-handler! signal/chld void))
    (initialize "locale"
      (foreign-code "setlocale(LC_CTYPE, \"\");")
      (foreign-code "setlocale(LC_COLLATE, \"\");"))
    (initialize "plugins"
      (load-plugins (read-plugins)))
    (initialize "config"
      (handle-exceptions x
        (log-write! 'warning (format "failed to load ~a" *sysrc-path*))
        (user-load *sysrc-path*))
      (let ((config (alist-ref 'config opts eqv? *scmusrc-path*)))
        (handle-exceptions x
          (log-write! 'error (format "failed to load ~a" config))
          (user-load config))))
    (initialize "curses"
      (init-curses))
    (initialize "mpd connection"
      (connect! (alist-ref 'address opts)
                (alist-ref 'port opts eqv? 'default)
                (alist-ref 'password opts eqv? 'default)))))

;; enter main loop, and clean up on exit
(let ((code (call/cc main)))
  (exit-all)
  (when *error*
    (format (current-error-port) "Unexpected error.  Exiting.~n")
    (pp (condition->list *error*) (current-error-port)))
  (exit code))
