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
(define *mpd-status*)

(define (seconds->string total-seconds)
  (let* ((total-minutes (quotient total-seconds 60))
         (seconds (modulo total-seconds 60))
         (minutes (modulo total-minutes 60))
         (hours (quotient total-minutes 60)))
    (string-append (if (= hours 0)
                     ""
                     (format "~a:" hours))
                   (if (< minutes 10)
                     (format "0~a:" minutes)
                     (number->string minutes))
                   (if (< seconds 10)
                     (format "0~a" seconds)
                     (number->string seconds)))))

;; initialize the mpd connection, printing a message on failure
(define (init-client host port)
  (condition-case
    (begin
      (set! *mpd-connection* (mpd:connect host port))
      (scmus-update-status!))
    (ex (exn i/o) (printf "Error: failed connecting to ~a:~a~n" host port)
                  (abort ex))))

(define (exit-client)
  (mpd:disconnect *mpd-connection*))

(define (scmus-playing?)
  (eqv? (scmus-state) 'play))

(define (scmus-elapsed)
  (seconds->string (car (scmus-time))))

(define (scmus-update-status!)
  (condition-case
    (set! *mpd-status* (mpd:get-status *mpd-connection*))
    (e () (curses-print "STATUS UPDATE ERROR")))
  *mpd-status*)

(define (scmus-status) *mpd-status*)
(define (scmus-volume) (alist-ref 'volume *mpd-status*))
(define (scmus-repeat?) (alist-ref 'repeat *mpd-status*))
(define (scmus-random?) (alist-ref 'random *mpd-status*))
(define (scmus-single) (alist-ref 'single *mpd-status*))
(define (scmus-consume) (alist-ref 'consume *mpd-status*))
(define (scmus-playlist) (alist-ref 'playlist *mpd-status*))
(define (scmus-playlist-length) (alist-ref 'playlistlength *mpd-status*))
(define (scmus-xfade) (alist-ref 'xfade *mpd-status*))
(define (scmus-mixrampdb) (alist-ref 'mixrampdb *mpd-status*))
(define (scmus-mixrampdelay) (alist-ref 'mixrampdelay *mpd-status*))
(define (scmus-state) (alist-ref 'state *mpd-status*))
(define (scmus-song) (alist-ref 'song *mpd-status*))
(define (scmus-song-id) (alist-ref 'songid *mpd-status*))
(define (scmus-time) (alist-ref 'time *mpd-status*))
(define (*scmus-elapsed) (alist-ref 'elapsed *mpd-status*))
(define (scmus-bitrate) (alist-ref 'bitrate *mpd-status*))
(define (scmus-audio) (alist-ref 'audio *mpd-status*))
(define (scmus-next-song) (alist-ref 'nextsong *mpd-status*))
(define (scmus-next-song-id) (alist-ref 'nextsongid *mpd-status*))

(define (scmus-next!) (mpd:next-song! *mpd-connection*))
(define (scmus-prev!) (mpd:previous-song! *mpd-connection*))
(define (scmus-play! id) (mpd:play! *mpd-connection* id))
(define (scmus-pause!) (mpd:pause! *mpd-connection*))
(define (scmus-stop!) (mpd:stop! *mpd-connection*))
