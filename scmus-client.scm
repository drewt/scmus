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

(require-extension srfi-18)

(declare (unit scmus-client)
         (uses mpd-client)
         (hide scmus-try-reconnect
               status-selector
               track-selector
               stat-selector
               scmus-command))

(define *mpd-connection* #f)
(define *mpd-status* '())
(define *mpd-stats* '())
(define *current-track* '())
(define *queue* '())

(define *last-update* -1.0)

;; initialize the mpd connection, printing a message on failure
(define (init-client host port)
  (assert (string? host))
  (assert (integer? port))
  (condition-case
    (begin
      (set! *mpd-connection* (mpd:connect host port))
      (set! *mpd-stats* (mpd:get-stats *mpd-connection*))
      (scmus-update-status!))
    (ex (exn i/o) (printf "Error: failed connecting to ~a:~a~n" host port)
                  (abort ex))))

(define (exit-client)
  (if *mpd-connection*
    (mpd:disconnect *mpd-connection*)))

(define (scmus-try-reconnect)
  (condition-case
    (set! *mpd-connection* (mpd:reconnect *mpd-connection*))
    (e () (error-set! e))))

(define (scmus-update-status!)
  (let ((ct (time->seconds (current-time)))
        (version (scmus-queue-version)))
    (when (> (- ct *last-update*) 0.5)
      (condition-case
        (begin
          (set! *mpd-status* (mpd:get-status *mpd-connection*))
          (register-event! 'status-changed)
          (unless (= (scmus-song-id) (track-id *current-track*))
            (set! *current-track* (mpd:get-current-song *mpd-connection*))
            (register-event! 'queue-changed)
            (register-event! 'current-line-changed))
          (when (not (= version (scmus-queue-version)))
            (set! *queue* (mpd:list-queue *mpd-connection*))
            (register-event! 'queue-data-changed))
          (set! *last-update* ct))
        (e () (error-set! e)
              (scmus-try-reconnect))))))

(define (scmus-elapsed)
  (seconds->string (*scmus-elapsed)))

(define-syntax status-selector
  (syntax-rules ()
    ((status-selector name sym)
      (status-selector name sym ""))
    ((status-selector name sym default)
      (define (name)
        (let ((e (alist-ref sym *mpd-status*)))
          (if e e default))))))

(define-syntax track-selector
  (syntax-rules ()
    ((track-selector name sym)
      (track-selector name sym ""))
    ((track-selector name sym default)
      (define (name song)
        (assert (list? song))
        (let ((e (alist-ref sym song)))
         (if e e default))))))

(define-syntax stat-selector
  (syntax-rules ()
    ((stat-selector name sym)
      (define (name)
        (alist-ref sym *mpd-stats*)))))

(define (scmus-status) *mpd-status*)
(status-selector scmus-volume 'volume 0)
(status-selector scmus-repeat? 'repeat #f)
(status-selector scmus-random? 'random #f)
(status-selector scmus-single? 'single #f)
(status-selector scmus-consume? 'consume #f)
(status-selector scmus-queue-version 'queue-version 0)
(status-selector scmus-queue-length 'queue-length 0)
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

(define (current-track? track)
  (= (track-id track) (track-id *current-track*)))

(define (track= a b)
  (= (track-id a) (track-id b)))

(stat-selector scmus-artists 'artists)
(stat-selector scmus-albums 'albums)
(stat-selector scmus-songs 'songs)
(stat-selector scmus-uptime 'uptime)
(stat-selector scmus-playtime 'playtime)
(stat-selector scmus-db-playtime 'db-playtime)
(stat-selector scmus-db-update 'db-update)

(define-syntax scmus-command
  (syntax-rules (0 1 2)
    ((scmus-command 0 name mpd-fn)
      (define (name)
        (condition-case (mpd-fn *mpd-connection*)
          (e (mpd) (error-set! e)))))
    ((scmus-command 1 name mpd-fn)
      (define (name arg)
        (condition-case (mpd-fn *mpd-connection* arg)
          (e (mpd) (error-set! e)))))
    ((scmus-command 2 name mpd-fn)
      (define (name arg1 arg2)
        (condition-case (mpd-fn *mpd-connection* arg1 arg2)
          (e (mpd) (error-set! e)))))))

(scmus-command 0 scmus-next! mpd:next-song!)
(scmus-command 0 scmus-prev! mpd:previous-song!)
(scmus-command 0 scmus-play! mpd:play!)
(scmus-command 1 scmus-play-id! mpd:play-id!)
(scmus-command 1 scmus-play-pos! mpd:play-pos!)
(scmus-command 0 scmus-pause! mpd:pause!)
(scmus-command 0 scmus-stop! mpd:stop!)
(scmus-command 2 scmus-seek-id! mpd:seek-id!)
(scmus-command 2 scmus-seek-pos! mpd:seek-pos!)
(scmus-command 1 scmus-repeat-set! mpd:repeat-set!)
(scmus-command 1 scmus-random-set! mpd:random-set!)
(scmus-command 1 scmus-single-set! mpd:single-set!)
(scmus-command 1 scmus-consume-set! mpd:consume-set!)

(define (scmus-play-track! track)
  (assert (>= (track-id track) 0))
  (scmus-play-id! (track-id track)))

(define (scmus-seek! seconds)
  (assert (integer? seconds))
  (scmus-seek-id! (track-id *current-track*)
                  (min (track-duration *current-track*)
                       (max 0 (+ (*scmus-elapsed) seconds)))))

(define (scmus-toggle-repeat!)
  (scmus-repeat-set! (if (scmus-repeat?) #f #t)))
(define (scmus-toggle-random!)
  (scmus-random-set! (if (scmus-random?) #f #t)))
(define (scmus-toggle-single!)
  (scmus-single-set! (if (scmus-single?) #f #t)))
(define (scmus-toggle-consume!)
  (scmus-consume-set! (if (scmus-consume?) #f #t)))
