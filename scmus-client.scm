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
         (uses mpd-client)
         (hide scmus-try-reconnect
               status-selector
               current-selector
               track-selector
               stat-selector
               scmus-command))

(define *mpd-connection* #f)
(define *mpd-status* '())
(define *mpd-stats* '())
(define *current-track* '())

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
                     (format "~a:" minutes))
                   (if (< seconds 10)
                     (format "0~a" seconds)
                     (number->string seconds)))))

;; initialize the mpd connection, printing a message on failure
(define (init-client host port)
  (condition-case
    (begin
      (set! *mpd-connection* (mpd:connect host port))
      (set! *mpd-stats* (mpd:get-stats *mpd-connection*))
      (scmus-update-status!))
    (ex (exn i/o) (printf "Error: failed connecting to ~a:~a~n" host port)
                  (abort ex))))

(define (exit-client)
  (mpd:disconnect *mpd-connection*))

(define (scmus-try-reconnect)
  (condition-case
    (set! *mpd-connection* (mpd:reconnect *mpd-connection*))
    (e () (void))))

(define (scmus-update-status!)
  (condition-case
    (begin
      (set! *mpd-status* (mpd:get-status *mpd-connection*))
      (ui-element-changed! 'status-line)
      (when (not (= (scmus-song-id) (current-id)))
        (set! *current-track* (mpd:get-current-song *mpd-connection*))
        (ui-element-changed! 'current-line)))
    (e () (scmus-try-reconnect)))
  *mpd-status*)

(define (scmus-elapsed)
  (seconds->string (*scmus-elapsed)))

(define-syntax status-selector
  (syntax-rules ()
    ((status-selector name sym)
      (status-selector name sym ""))
    ((status-selector name sym default)
      (define (name)
        (let ((e (alist-ref sym *mpd-status*)))
          (let ((p (open-output-string)))
            (pp *current-track* p)
            (curses-print (get-output-string p))
            )
          (if e e default))))))

(define-syntax current-selector
  (syntax-rules ()
    ((current-selector name sym)
      (current-selector name sym ""))
    ((current-selector name sym default)
      (define (name)
        (let ((e (alist-ref sym *current-track*)))
          (if e e default))))))

(define-syntax track-selector
  (syntax-rules ()
    ((track-selector name sym)
      (track-selector name sym ""))
    ((track-selector name sym default)
      (define (name song)
        (let ((e (alist-ref sym song)))
         (if e e default))))))

(define-syntax stat-selector
  (syntax-rules ()
    ((stat-selector name sym)
      (define (name)
        (alist-ref sym *mpd-stats*)))))

(define (scmus-status) *mpd-status*)
(status-selector scmus-volume 'volume 0)
(status-selector scmus-repeat? 'repeat)
(status-selector scmus-random? 'random)
(status-selector scmus-single 'single)
(status-selector scmus-consume 'consume)
(status-selector scmus-playlist 'queue-version 0)
(status-selector scmus-playlist-length 'queue-length 0)
(status-selector scmus-xfade 'xfade 0)
(status-selector scmus-mixrampdb 'mixrampdb 0.0)
(status-selector scmus-mixrampdelay 'mixrampdelay 0.0)
(status-selector scmus-state 'state 'unknown)
(status-selector scmus-song 'song-pos -1)
(status-selector scmus-song-id 'song-id -1)
(status-selector *scmus-elapsed 'elapsed-time 0)
(status-selector scmus-elapsed-ms 'elapsed-ms 0)
(status-selector scmus-total-time 'total-time 0)
(status-selector scmus-bitrate 'bitrate 0)
;(status-selector scmus-audio 'audio '(0 0 0))
(status-selector scmus-next-song 'next-song-pos 0)
(status-selector scmus-next-song-id 'next-song-id 0)

(track-selector track-file 'file)
(track-selector track-artist 'artist)
(track-selector track-album 'album)
(track-selector track-albumartist 'albumartist)
(track-selector track-title 'title)
(track-selector track-track 'track)
(track-selector track-name 'name)
(track-selector track-genre 'genre)
(track-selector track-date 'date)
(track-selector track-composer 'composer)
(track-selector track-performer 'performer)
(track-selector track-disc 'disc "1")
(track-selector track-duration 'duration 0)
(track-selector track-start 'start 0)
(track-selector track-end 'end 0)
(track-selector track-last-modified 'last-modified)
(track-selector track-pos 'pos -1)
(track-selector track-id 'id -1)
(track-selector track-prio 'prio 0)

(current-selector current-file 'file)
(current-selector current-last-modified 'last-modified)
(current-selector current-duration 'duration 0)
(current-selector current-title 'title)
(current-selector current-artist 'artist)
(current-selector current-date 'date)
(current-selector current-album 'album)
(current-selector current-track 'track)
(current-selector current-albumartist 'albumartist)
(current-selector current-pos 'pos 0)
(current-selector current-id 'id -1)
(current-selector current-prio 'prio 0)

(stat-selector scmus-artists 'artists)
(stat-selector scmus-albums 'albums)
(stat-selector scmus-songs 'songs)
(stat-selector scmus-uptime 'uptime)
(stat-selector scmus-playtime 'playtime)
(stat-selector scmus-db-playtime 'db-playtime)
(stat-selector scmus-db-update 'db-update)

(define-syntax scmus-command
  (syntax-rules ()
    ((scmus-command name mpd-fn)
      (define (name)
        (condition-case
          (mpd-fn *mpd-connection*)
          (e (mpd)
             (curses-print (mpd:error-message e))))))))

(scmus-command scmus-next! mpd:next-song!)
(scmus-command scmus-prev! mpd:previous-song!)
(scmus-command scmus-play! mpd:play!)
(scmus-command scmus-pause! mpd:pause!)
(scmus-command scmus-stop! mpd:stop!)
