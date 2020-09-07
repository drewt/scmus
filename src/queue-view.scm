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

(declare (hide))

(import (scmus base)
        (scmus client)
        (scmus event)
        (scmus option)
        (scmus status)
        (scmus track)
        (scmus tui)
        (scmus view)
        (scmus widgets))

(define queue-cursed
  (win-cursed-fun (lambda (row) (current-track? (window-row-data row)))))

(add-option-listener 'format-queue
  (lambda (_) (widget-invalidate queue-widget)))

(define (queue-make-rows)
  (map (lambda (x) (make-window-row x 'file 'format-queue))
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

(define queue-widget
  (make <queue-window>
        'data       (queue-make-rows)
        'cursed     CURSED-WIN
        'cursed-fun queue-cursed))

(define-view queue
  (make-frame
    'body   queue-widget
    'header (make-format-text " Queue - ~{queue-length} tracks" '() 'cursed CURSED-WIN-TITLE)))

(add-listener/global 'queue-changed
  (lambda ()
    (widget-damaged! (get-view 'queue))))

(add-listener/global 'queue-data-changed
  (lambda ()
    (set! (list-box-data queue-widget) (queue-make-rows))))
