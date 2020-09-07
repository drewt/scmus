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

(module (scmus event)
    (<event-source>
     current-event-source
     add-listener
     add-listener/global
     signal-event
     signal-event/global
     run-later
     handle-events
     register-timer!
     register-timer-event!)
  (import coops
          (scmus base))

  (define *deferred-events* '())

  (define-class <event-source> ()
    ((listeners initform: '()
                accessor: event-source-listeners)))

  (define current-event-source (make-parameter #f))

  (define-method (add-listener (src <event-source>) type listener)
    (set! (event-source-listeners src) (cons (cons type listener)
                                             (event-source-listeners src))))

  (define-method (signal-event (src <event-source>) type #!key (args '()))
    (for-each (lambda (type/listener)
                (when (eqv? (car type/listener) type)
                  (set! *deferred-events*
                    (cons (lambda ()
                            (parameterize ((current-event-source src))
                              (apply (cdr type/listener) args)))
                          *deferred-events*))))
              (event-source-listeners src)))

  (define *global-event-source* (make <event-source>))

  (define (add-listener/global type listener)
    (add-listener *global-event-source* type listener))

  (define (signal-event/global type . args)
    (apply signal-event *global-event-source* type args))

  (define (run-later thunk)
    (set! *deferred-events* (cons thunk *deferred-events*)))

  (define (handle-events)
    (handle-timers)
    ;; XXX: Events may trigger other events, so we need to loop until the queue
    ;;      is really empty.  To avoid hanging in the case where multiple events
    ;;      endlessly trigger each other, we specify a maximum recursion depth
    ;;      and discard all pending events when it's reached.
    (let loop ((events *deferred-events*) (i 0))
      (set! *deferred-events* '())
      (if (> i 10)
        (make-property-condition 'exn
          'message "handle-events reached depth limit"
          'arguments events)
        (begin
          (for-each (lambda (ev) (ev)) events)
          (if (null? *deferred-events*)
            #f
            (loop *deferred-events* (+ i 1)))))))

  (define *timers* '())

  (define timer-expire-time car)
  (define timer-thunk cdr)

  (define (register-timer! thunk seconds #!key (recurring #f))
    (let ((timer (cons (+ (current-second/precise)
                          seconds)
                       (if recurring
                           (rec (recurring-thunk)
                             (thunk)
                             (register-timer! recurring-thunk seconds))
                           thunk))))
      (set! *timers* (append! *timers* (list timer)))))

  (define (register-timer-event! name . rest)
    (apply register-timer! (lambda () (signal-event/global name)) rest))

  (define (handle-timers)
    (let ((ct (current-second/precise)))
      (let loop ((timers *timers*) (thunks '()))
        (if (or (null? timers)
                (> (timer-expire-time (car timers)) ct))
          (begin
            (set! *timers* timers)
            (for-each (lambda (thunk) (thunk)) (reverse thunks)))
          (loop (cdr timers)
                (cons (cdar timers) thunks)))))))
