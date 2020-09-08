;;
;; Copyright 2014-2020 Drew Thoreson
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

(include "test/test.scm")
(declare (unit test/event)
         (uses event))

(import scmus.event)

(start-test "event")

(define-class <test-es> (<event-source>)
  ((ran? initform: #f
         accessor: listener-ran?)))

;; add-listener, signal-event
(let ((src (make <test-es>)))
  (add-listener src 'type (lambda () (set! (listener-ran? src) #t)))

  ; signal an unlistened event type
  (signal-event src 'not-type)
  (handle-events)
  (check (listener-ran? src) => #f)
  
  ; signal the listened event type
  (signal-event src 'type)
  (check (listener-ran? src) => #f)
  (handle-events)
  (check (listener-ran? src) => #t))

;; add-listener/global, signal-event/global
(let ((ran? #f))
  (add-listener/global 'type (lambda () (set! ran? #t)))
  (signal-event/global 'type)
  (check ran? => #f)
  (handle-events)
  (check ran? => #t))

;; event cascade
(let ((first-ran? #f)
      (second-ran? #f))
  (add-listener/global 'first (lambda ()
                                (set! first-ran? #t)
                                (signal-event/global 'second)))
  (add-listener/global 'second (lambda () (set! second-ran? #t)))
  (signal-event/global 'first)
  (handle-events)
  (check first-ran? => #t)
  (check second-ran? => #t))

;; run-later
(let ((ran? #f))
  (run-later (lambda () (set! ran? #t)))
  (check ran? => #f)
  (handle-events)
  (check ran? => #t))

;; TODO: test timers

(end-test "event")
