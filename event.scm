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

(: *events* (list-of symbol))
(define *events* '())

(: *event-handlers* (list-of (pair symbol thunk)))
(define *event-handlers* '())

(: register-event! (symbol -> undefined))
(define (register-event! event)
  (if (alist-ref event *event-handlers*)
    (set! *events* (cons event *events*))
    (error-set! (make-property-condition 'exn
                  'message "No event handler registered for event"
                  'arguments event))))

(: register-event-handler! (symbol thunk -> undefined))
(define (register-event-handler! event handler)
  (set! *event-handlers* (cons (cons event handler) *event-handlers*)))

(: handle-events! thunk)
(define (handle-events!)
  (for-each (lambda (x)
              ((alist-ref x *event-handlers*)))
            *events*)
  (set! *events* '()))
