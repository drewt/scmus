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

(module scmus.log
        (<logger>
         <port-logger>
         current-logger
         log-read
         log-write! 
         logger-append-entry!
         logger-size
         logger-read
         logger-write!
         make-logger)
  (import coops
          coops-utils
          scmus.base)

  (define-class <log-entry> ()
    ((type          initform: '?
                    reader:   log-entry-type)
     (short-message initform: ""
                    reader:   log-entry-short-message)
     (long-message  initform: #f
                    reader:   log-entry-long-message)))

  (define (make-log-entry type short-message long-message)
    (make <log-entry> 'type type 'short-message short-message 'long-message long-message))

  (define-method (print-object (entry <log-entry>) out)
    (format out "<~a> ~a~n"
                (log-entry-type entry)
                (log-entry-short-message entry))
    (when (log-entry-long-message entry)
      (for-each (lambda (line) (format out "    ~a~n" line))
                (string-split-lines (log-entry-long-message entry)))))

  ;; Base logger class.  Keeps N entries in an in-memory log.
  (define-class <logger> ()
    ((log    initform: '()
             accessor: logger-log)
     (size   initform: 25
             accessor: logger-size)
     ; Function taking a log-entry type and returning a boolean.
     ; Log entries for which this function returns #f are discarded.
     (filter initform: (lambda (_) #t)
             accessor: logger-filter)))

  (define (make-logger logger-class size)
    (make logger-class 'size size))

  ;; Read N entries of type TYPE from the logger.
  ;; If TYPE is not provided, entries of all types are returned.
  ;; If N is not provided, the entire log is returned.
  (define-method (logger-read (logger <logger>) #!key type n)
    (let ((entries (reverse
                     (if type
                       (filter (lambda (x) (equal? (log-entry-type x) type))
                               (logger-log logger))
                       (logger-log logger)))))
      (if n (list-truncate entries n) entries)))

  ;; Append ENTRY to LOGGER.  This is the method responsible for log rotation.
  (define-method (logger-append-entry! (logger <logger>) entry)
    (set! (logger-log logger) (list-truncate (cons entry (logger-log logger))
                                             (logger-size logger))))

  ;; Write a log entry to LOGGER.  This is the method responsible for ingress filtering.
  (define-method (logger-write! (logger <logger>) type short-message #!optional long-message)
    (when ((logger-filter logger) type)
      (logger-append-entry! logger (make-log-entry type short-message long-message))))

  (define-class <port-logger> (<logger>)
    ((port initform: (current-error-port)
           accessor: port-logger-port)))

  (define-method (logger-append-entry! after: (logger <port-logger>) entry)
    (display entry (port-logger-port logger)))

  (define current-logger
    (make-parameter (make <logger>)
                    (lambda (x)
                      (if (instance-of? x <logger>)
                        x
                        (current-logger)))))

  (define (log-read #!key type n)
    (logger-read (current-logger) type n))

  (define (log-write! type short-message #!optional long-message)
    (logger-write! (current-logger) type short-message long-message)))
