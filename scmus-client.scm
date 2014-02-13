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

(declare (unit scmus-client)
         (uses mpd-client))

(define *mpd-connection*)

;; initialize the mpd connection, printing a message on failure
(define (init-client host port)
  (condition-case (set! *mpd-connection* (mpd:connect host port))
    (ex (exn i/o) (printf "Error: failed connecting to ~a:~a~n" host port)
                  (abort ex))))

(define (exit-client)
  (mpd:disconnect *mpd-connection*))

(define (scmus-next!) (mpd:next-song! *mpd-connection*))
(define (scmus-prev!) (mpd:previous-song! *mpd-connection*))
(define (scmus-play! id) (mpd:play! *mpd-connection* id))
(define (scmus-pause!) (mpd:pause! *mpd-connection*))
(define (scmus-stop!) (mpd:stop! *mpd-connection*))
