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

(import scmus.base scmus.client scmus.event scmus.status scmus.track
        scmus.tui)

(: queue-remove! (window -> undefined))
(define (queue-remove! window)
  (let loop ((marked (sort (window-marked window) >)))
    (unless (null? marked)
      (scmus-delete! (car marked))
      (loop (cdr marked))))
  (window-clear-marked! window)
  (scmus-update-queue!))

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
  (window-clear-marked! window)
  (scmus-update-queue!))

(define-view queue
  (make-frame
    'body   (make-window 'data       (list-of 'file (current-queue))
                         'activate   (lambda (w) (scmus-play-track! (cdr (window-selected w))))
                         'match      (lambda (row query) (track-match (cdr row) query))
                         'remove     queue-remove!
                         'clear      (lambda (w) (scmus-clear!))
                         'move       queue-move!
                         'format     (lambda (tag) (get-format 'format-queue))
                         'cursed     CURSED-WIN
                         'cursed-fn  (win-cursed-fn (lambda (row) (current-track? (cdr row)))))
    'header (make-format-text " Queue - ~{queue-length} tracks" '() 'cursed CURSED-WIN-TITLE)))

(define-event-handler (queue-changed) ()
  (widget-damaged! (get-view 'queue)))

(define-event-handler (queue-data-changed) ()
  (set! (window-data (get-window 'queue)) (list-of 'file (current-queue))))
