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

(import scmus.base
        scmus.client
        scmus.event
        scmus.status
        scmus.track
        scmus.tui
        scmus.view
        scmus.widgets)

(define queue-cursed
  (win-cursed-fun (lambda (row) (current-track? (window-row-data row)))))

(define (queue-format _)
  (get-format 'format-queue))

(define (queue-make-rows)
  (map (lambda (x) (make-window-row x 'file queue-format))
       (current-queue)))

(define-class <queue-window> (<window>))

(define-method (widget-activate (window <queue-window>))
  (unless (list-box-empty? window)
    (scmus-play-track! (window-row-data (list-box-selected window)))))

(define-method (widget-remove (window <queue-window>))
  (for-each scmus-delete! (sort (window-marked window) >))
  (widget-clear-marked window)
  (scmus-update-queue!))

(define-method (widget-clear (window <queue-window>))
  (scmus-clear!))

(define-method (widget-paste (window <queue-window>) before?)
  (let loop ((marked (sort (*window-marked window) <))
             (pos    (- (list-box-sel-pos window)
                        (if before? 1 0))))
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
  (widget-clear-marked window)
  (scmus-update-queue!))

(define *queue-window*
  (make <queue-window>
        'data       (queue-make-rows)
        'cursed     CURSED-WIN
        'cursed-fun queue-cursed))

(define-view queue
  (make-frame
    'body   *queue-window*
    'header (make-format-text " Queue - ~{queue-length} tracks" '() 'cursed CURSED-WIN-TITLE)))

(define-event-handler (queue-changed) ()
  (widget-damaged! (get-view 'queue)))

(define-event-handler (queue-data-changed) ()
  (set! (list-box-data *queue-window*) (queue-make-rows)))
