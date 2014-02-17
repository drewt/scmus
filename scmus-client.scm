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
(define *current-song* #f)

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

(define (scmus-update-status!)
  (condition-case
    (begin
      (set! *mpd-status* (mpd:get-status *mpd-connection*))
      (if (not (= (scmus-song-id) (current-id)))
        (set! *current-song* (mpd:get-current-song *mpd-connection*))))
    (e () (curses-print "STATUS UPDATE ERROR")))
  *mpd-status*)

(define (scmus-elapsed)
  (seconds->string (car (scmus-time))))

(define-syntax status-selector
  (syntax-rules ()
    ((status-selector name sym)
      (status-selector name sym ""))
    ((status-selector name sym default)
      (define (name)
        (let ((e (alist-ref sym *mpd-status*)))
          (if e e default))))))

(define-syntax current-selector
  (syntax-rules ()
    ((current-selector name sym)
      (current-selector name sym ""))
    ((current-selector name sym default)
      (define (name)
        (let ((e (alist-ref sym *current-song*)))
          (if e e default))))))

(define (scmus-status) *mpd-status*)
(status-selector scmus-volume 'volume 0)
(status-selector scmus-repeat? 'repeat)
(status-selector scmus-random? 'random)
(status-selector scmus-single 'single)
(status-selector scmus-consume 'consume)
(status-selector scmus-playlist 'playlist 0)
(status-selector scmus-playlist-length 'playlistlength 0)
(status-selector scmus-xfade 'xfade 0)
(status-selector scmus-mixrampdb 'mixrampdb)
(status-selector scmus-mixrampdelay 'mixrampdelay)
(status-selector scmus-state 'state 'unknown)
(status-selector scmus-song 'song 0)
(status-selector scmus-song-id 'songid 0)
(status-selector scmus-time 'time '(0 0))
(status-selector *scmus-elapsed 'elapsed)
(status-selector scmus-bitrate 'bitrate 0)
(status-selector scmus-audio 'audio '(0 0 0))
(status-selector scmus-next-song 'nextsong)
(status-selector scmus-next-song-id 'nextsongid)

(current-selector current-file 'file)
(current-selector current-last-modified 'Last-Modified)
(current-selector current-time 'Time 0)
(current-selector current-title 'Title)
(current-selector current-artist 'Artist)
(current-selector current-date 'Date)
(current-selector current-album 'Album)
(current-selector current-track 'Track 0)
(current-selector current-albumartist 'AlbumArtist)
(current-selector current-pos 'Pos 0)
(current-selector current-id 'Id 0)

(define (scmus-next!) (mpd:next-song! *mpd-connection*))
(define (scmus-prev!) (mpd:previous-song! *mpd-connection*))
(define (scmus-play! id) (mpd:play! *mpd-connection* id))
(define (scmus-pause!) (mpd:pause! *mpd-connection*))
(define (scmus-stop!) (mpd:stop! *mpd-connection*))
