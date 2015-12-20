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
  (let ((handlers (alist-ref event *event-handlers* eqv? '())))
    (set! *event-handlers*
      (alist-update! event (cons handler handlers) *event-handlers*))))

(: handle-events! thunk)
(define (handle-events!)
  (define (handle-event event)
    (let loop ((handlers (reverse (alist-ref event *event-handlers* eqv? '()))))
      (unless (null? handlers)
        ((car handlers))
        (loop (cdr handlers)))))
  ;; XXX: Events may trigger other events, so we need to loop until the queue
  ;;      is really empty.  To avoid hanging in the case where multiple events
  ;;      endlessly trigger each other, we specify a maximum recursion depth
  ;;      and discard all pending events when it's reached.
  (let loop ((events *events*) (i 0))
    (set! *events* '())
    (if (> i 10)
      (error-set! (make-property-condition 'exn
                    'message "handle-events! reached depth limit"
                    'arguments events))
      (begin
        (for-each handle-event events)
        (unless (null? *events*)
          (loop *events* (+ i 1)))))))
