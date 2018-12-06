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

(declare (export make-queue-view))

(import scmus.base
        scmus.client
        scmus.event
        scmus.status
        scmus.track
        scmus.tui
        scmus.widgets)

(define (queue-activate! w)
  (scmus-play-track! (window-row-data (window-selected w))))

(define (queue-match row query)
  (track-match (window-row-data row) query))

(define (queue-remove! window)
  (let loop ((marked (sort (window-marked window) >)))
    (unless (null? marked)
      (scmus-delete! (car marked))
      (loop (cdr marked))))
  (window-clear-marked! window)
  (scmus-update-queue!))

(define (queue-move! window before)
  (let loop ((marked (sort (*window-marked window) <))
             (pos    (- (window-sel-pos window)
                        (if before 1 0))))
    (unless (null? marked)
      (if (< (car marked) pos)
        (begin
          (scmus-move! (car marked) pos)
          (loop (map (lambda (x) (if (< x pos) (- x 1) x))
                     (cdr marked))
                pos))
        (begin
          (scmus-move! (car marked) (+ 1 pos))
          (loop (cdr marked) (+ 1 pos))))))
  (window-clear-marked! window)
  (scmus-update-queue!))

(define queue-cursed
  (win-cursed-fn (lambda (row) (current-track? (window-row-data row)))))

(define (queue-format _)
  (get-format 'format-queue))

(define (queue-make-rows)
  (map (lambda (x) (make-window-row x 'file queue-format))
       (current-queue)))

(define-view queue
  (make-frame
    'body (make-window 'data      (queue-make-rows)
                       'activate  queue-activate!
                       'match     queue-match
                       'remove    queue-remove!
                       'clear     (lambda (w) (scmus-clear!))
                       'move      queue-move!
                       'cursed    CURSED-WIN
                       'cursed-fn queue-cursed)
    'header (make-format-text " Queue - ~{queue-length} tracks" '() 'cursed CURSED-WIN-TITLE)))

(define-event-handler (queue-changed) ()
  (widget-damaged! (get-view 'queue)))

(define-event-handler (queue-data-changed) ()
  (set! (window-data (get-window 'queue)) (queue-make-rows)))
