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
  (uses scmus-client ui-curses window)
  (export make-queue-view))

(define (make-queue-view)
  (make-view (make-window #f
                          (lambda (w) *queue*)
                          (lambda (w) (register-event! 'queue-changed))
                          (lambda (w) (scmus-play-track! (window-selected w)))
                          void
                          track-match)
             "Queue - ~{queue-length} tracks"
             (lambda (window track line-nr cursed)
               (track-print-line line-nr (get-format 'format-queue) track cursed))
             cursed: (win-cursed-fn current-track?)))
