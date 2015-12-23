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

(declare (unit queue-view)
  (uses event option scmus-client ui-lib view window)
  (export make-queue-view))

(: queue-remove! (window -> undefined))
(define (queue-remove! window)
  (let loop ((marked (sort (window-marked window) >)))
    (unless (null? marked)
      (scmus-delete! (car marked))
      (loop (cdr marked))))
  (window-clear-marked! window))

(define (*queue-move! marked pos)
  (unless (null? marked)
    (if (< (car marked) pos)
      (begin
        (scmus-move! (car marked) pos)
        (*queue-move! (map (lambda (x)
                             (if (< x pos)
                               (- x 1)
                               x))
                           (cdr marked))
                      pos))
      (begin
        (scmus-move! (car marked) (+ 1 pos))
        (*queue-move! (cdr marked) (+ 1 pos))))))

(: queue-move! (window boolean -> undefined))
(define (queue-move! window before)
  (*queue-move! (sort (*window-marked window) <)
                (- (window-sel-pos window)
                   (if before 1 0)))
  (window-clear-marked! window))

(: queue-print-line (window track fixnum fixnum -> undefined))
(define (queue-print-line window track line-nr cursed)
  (track-print-line line-nr (get-format 'format-queue) track cursed))

(define-view queue
  (make-view (make-window 'data-thunk (lambda (w) *queue*)
                          'changed    (lambda (w) (register-event! 'queue-changed))
                          'activate   (lambda (w) (scmus-play-track! (window-selected w)))
                          'match      track-match)
             "Queue - ~{queue-length} tracks"
             print-line: queue-print-line
             cursed:     (win-cursed-fn current-track?)
             remove:     queue-remove!
             clear:      (lambda (w) (scmus-clear!))
             move:       queue-move!))

(define-event-handler (queue-changed)
  (update-view! 'queue))

(define-event-handler (queue-data-changed)
  (window-data-len-update! (get-window 'queue))
  (update-view! 'queue))
