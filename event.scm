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

(declare (unit event)
         (export handle-events!
                 register-event!
                 register-event-handler!))

(define *events* '())
(define *event-handlers* '())

(define (register-event! event)
  (assert (symbol? event) "register-event!" event)
  (set! *events* (cons event *events*)))

(define (register-event-handler! event handler)
  (set! *event-handlers* (cons (cons event handler) *event-handlers*)))

(define (handle-events!)
  (for-each (lambda (x)
              ((alist-ref x *event-handlers*)))
            *events*)
  (set! *events* '()))
