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

(require-extension srfi-13 srfi-18)

(declare (unit scmus-client)
         (uses mpd-client option)
         (hide scmus-try-reconnect status-selector track-selector stat-selector
               scmus-command))

(define *mpd-connection* #f)
(define *mpd-status* '())
(define *mpd-stats* '())
(define *current-track* '())
(define *queue* '())

(define *max-retries* 2)
(define *last-update* -1.0)

(define (scmus-connect! #!optional (*host #f) (*port #f) (*pass #f))
  (let ((host (if *host *host (get-option 'mpd-address)))
        (port (if *port *port (get-option 'mpd-port)))
        (pass (if *pass *pass (get-option 'mpd-password))))
    (assert (string? host) "scmus-connect!" host)
    (assert (integer? port) "scmus-connect!" port)
    (assert (or (not pass) (string? pass)) "scmus-connect!" pass)
    (condition-case
      (let ((con (mpd:connect host port pass)))
        (if (scmus-connected?)
          (mpd:disconnect *mpd-connection*))
        (set! *mpd-connection* con)
        (scmus-update-client!)
        (register-event! 'db-changed))
      (e (exn i/o net) (error-set! e) #f))))

(define (scmus-disconnect!)
  (mpd:disconnect *mpd-connection*)
  (set! *mpd-connection* #f))

(define (scmus-oneshot cmd . args)
  (condition-case
    (let* ((con (mpd:connect (get-option 'mpd-address)
                             (get-option 'mpd-port)))
           (res (apply mpd:send-command con cmd args)))
      (mpd:disconnect con)
      res)
    (e () (condition->list e))))

(define (scmus-connected?)
  (if *mpd-connection* #t #f))

(define (exit-client)
  (if *mpd-connection*
    (mpd:disconnect *mpd-connection*)))

(define (scmus-try-reconnect)
  (condition-case
    (begin (set! *mpd-connection* (mpd:reconnect *mpd-connection*)) #t)
    (e () (error-set! e) #f)))

(define (scmus-update-stats!)
  (set! *mpd-stats* (scmus-stats)))

(define (scmus-update-status!)
  (set! *mpd-status* (scmus-status))
  (register-event! 'status-changed))

(define (scmus-update-current-song!)
  (set! *current-track* (scmus-current-song))
  (register-event! 'queue-changed)
  (register-event! 'current-line-changed))

(define (scmus-update-queue!)
  (set! *queue* (scmus-playlist-info))
  (register-event! 'queue-data-changed))

(define (scmus-update-client!)
  (let ((ct (time->seconds (current-time)))
        (version (scmus-queue-version)))
    (when (and (scmus-connected?) (> (- ct *last-update*)
                                     (get-option 'status-update-interval)))
      (condition-case
        (begin
          (scmus-update-status!)
          (unless (= (scmus-song-id) (track-id *current-track*))
            (scmus-update-current-song!))
          (unless (= version (scmus-queue-version))
            (scmus-update-queue!))
          (set! *last-update* ct))
        (e () (error-set! e)
              (scmus-try-reconnect))))))

(define (scmus-elapsed)
  (seconds->string (inexact->exact (round (scmus-elapsed-ms)))))

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
        (assert (list? song) "" song)
        (let ((e (alist-ref sym song)))
         (if e e default))))))

(define-syntax stat-selector
  (syntax-rules ()
    ((stat-selector name sym)
      (stat-selector name sym 0))
    ((stat-selector name sym default)
      (define (name)
        (let ((e (alist-ref sym *mpd-stats*)))
          (if e e default))))))

(define (scmus-status) *mpd-status*)
(status-selector scmus-volume 'volume 0)
(status-selector scmus-repeat? 'repeat #f)
(status-selector scmus-random? 'random #f)
(status-selector scmus-single? 'single #f)
(status-selector scmus-consume? 'consume #f)
(status-selector scmus-queue-version 'playlist 0)
(status-selector scmus-queue-length 'playlistlength 0)
(status-selector scmus-state 'state 'unknown)
(status-selector scmus-song 'song -1)
(status-selector scmus-song-id 'songid -1)
(status-selector scmus-next-song 'nextsong 0)
(status-selector scmus-next-song-id 'nextsongid 0)
(status-selector scmus-elapsed-time 'time '(0))
(status-selector scmus-elapsed-ms 'elapsed 0)
(status-selector scmus-bitrate 'bitrate 0)
(status-selector scmus-xfade 'xfade 0)
(status-selector scmus-mixrampdb 'mixrampdb 0.0)
(status-selector scmus-mixrampdelay 'mixrampdelay 0.0)
;(status-selector scmus-total-time 'total-time 0)
;(status-selector scmus-audio 'audio '(0 0 0))

(track-selector track-file 'file)
(track-selector track-last-modified 'last-modified 0)
(track-selector *track-duration 'time '(0))
(track-selector track-title 'title)
(track-selector track-artist 'artist)
(track-selector track-album 'album)
(track-selector track-albumartist 'albumartist)
(track-selector track-genre 'genre)
(track-selector track-date 'date)
(track-selector track-track 'track)
(track-selector track-disc 'disc 1)
(track-selector track-name 'name)
(track-selector track-composer 'composer)
(track-selector track-performer 'performer)
(track-selector track-start 'start 0)
(track-selector track-end 'end 0)
(track-selector track-pos 'pos -1)
(track-selector track-id 'id -1)
(track-selector track-prio 'prio 0)

(define (track-duration track)
  (car (*track-duration track)))

(define (current-track)
  *current-track*)

(define (current-track? track)
  (track= track *current-track*))

(define (track= a b)
  (string=? (track-file a) (track-file b)))

(define (track-match track query)
  (or (string-contains-ci (track-title track) query)
      (string-contains-ci (track-album track) query)
      (string-contains-ci (track-artist track) query)
      (string-contains-ci (track-albumartist track) query)))

(stat-selector scmus-uptime 'uptime)
(stat-selector scmus-playtime 'playtime)
(stat-selector scmus-artists 'artists)
(stat-selector scmus-albums 'albums)
(stat-selector scmus-songs 'songs)
(stat-selector scmus-db-playtime 'db_playtime)
(stat-selector scmus-db-update 'db_update)

(define (scmus-bail! e)
  (scmus-disconnect!)
  (error-set! e)
  '())

(define (*scmus-command mpd-fn . args)
  (if (scmus-connected?)
    (condition-case (apply mpd-fn *mpd-connection* args)
      (e () (error-set! e)
            (unless (scmus-try-reconnect)
              (scmus-disconnect!))
            '()))
    '()))

(define-syntax scmus-command
  (syntax-rules ()
    ((scmus-command name mpd-fn)
      (define (name . args)
        (apply *scmus-command mpd-fn args)))))

(scmus-command scmus-clear-error! mpd:clear-error!)
(scmus-command scmus-current-song mpd:current-song)
(scmus-command scmus-status mpd:status)
(scmus-command scmus-stats mpd:stats)

(scmus-command scmus-consume-set! mpd:consume-set!)
(scmus-command scmus-random-set! mpd:random-set!)
(scmus-command scmus-repeat-set! mpd:repeat-set!)
(scmus-command scmus-single-set! mpd:single-set!)
(scmus-command scmus-crossfade-set! mpd:crossfade-set!)
(scmus-command scmus-mixrampdb-set! mpd:mixrampdb-set!)
(scmus-command scmus-mixrampdelay-set! mpd:mixrampdelay-set!)
(scmus-command scmus-volume-set! mpd:volume-set!)
(scmus-command scmus-replay-gain-mode-set! mpd:replay-gain-mode-set!)
(scmus-command scmus-replay-gain-status mpd:replay-gain-status)

(scmus-command scmus-next! mpd:next!)
(scmus-command scmus-pause! mpd:pause!)
(scmus-command scmus-toggle-pause! mpd:toggle-pause!)
(scmus-command scmus-play! mpd:play!)
(scmus-command scmus-play-pos! mpd:play-pos!)
(scmus-command scmus-play-id! mpd:play-id!)
(scmus-command scmus-prev! mpd:previous!)
(scmus-command scmus-seek-pos! mpd:seek!)
(scmus-command scmus-seek-id! mpd:seek-id!)
(scmus-command scmus-seek-cur! mpd:seek-cur!)
(scmus-command scmus-stop! mpd:stop!)

(scmus-command scmus-add! mpd:add!)
(scmus-command scmus-add-id! mpd:add-id!)
(scmus-command scmus-add-id-at! mpd:add-id-at!)
(scmus-command scmus-clear! mpd:clear!)
(scmus-command scmus-delete! mpd:delete!)
(scmus-command scmus-delete-id! mpd:delete-id!)
(scmus-command scmus-move! mpd:move!)
(scmus-command scmus-move-id! mpd:move-id!)
(scmus-command scmus-playlist-info mpd:playlist-info)
(scmus-command scmus-shuffle! mpd:shuffle!)
(scmus-command scmus-swap! mpd:swap!)
(scmus-command scmus-swap-id! mpd:swap-id!)

(scmus-command scmus-list-playlist mpd:list-playlist-info)
(scmus-command *scmus-list-playlists mpd:list-playlists)
(scmus-command scmus-playlist-load! mpd:playlist-load!)
(scmus-command scmus-playlist-add! mpd:playlist-add!)
(scmus-command scmus-playlist-clear! mpd:playlist-clear!)
(scmus-command scmus-playlist-delete! mpd:playlist-delete!)
(scmus-command scmus-playlist-move! mpd:playlist-move!)
(scmus-command scmus-playlist-rename! mpd:playlist-rename!)
(scmus-command scmus-playlist-rm! mpd:playlist-rm!)
(scmus-command scmus-playlist-save! mpd:playlist-save!)

(scmus-command scmus-count mpd:count)
(scmus-command scmus-find mpd:find)
(scmus-command scmus-find-add! mpd:find-add!)
(scmus-command scmus-list-tags mpd:list-tags)
(scmus-command scmus-list-all mpd:list-all)
(scmus-command scmus-list-all-info mpd:list-all-info)
(scmus-command scmus-list-files mpd:list-files)
(scmus-command scmus-lsinfo mpd:lsinfo)
(scmus-command scmus-read-comments mpd:read-comments)
(scmus-command scmus-search mpd:search)
(scmus-command scmus-search-add! mpd:search-add!)
(scmus-command scmus-search-add-pl! mpd:search-add-pl!)
(scmus-command scmus-update! mpd:update!)
(scmus-command scmus-rescan! mpd:rescan!)

(define (scmus-list-playlists)
  (filter (lambda (x) (eqv? (car x) 'playlist))
          (*scmus-list-playlists)))

(define (scmus-play-track! track)
  (assert (>= (track-id track) 0) "scmus-play-track!" (track-id track))
  (scmus-play-id! (track-id track)))

(define (scmus-seek! seconds)
  (assert (integer? seconds) "scmus-seek!" seconds)
  (scmus-seek-id! (track-id *current-track*)
                  (min (track-duration *current-track*)
                       (max 0 (+ (car (scmus-elapsed-time)) seconds)))))

(define (scmus-toggle-repeat!)
  (scmus-repeat-set! (if (scmus-repeat?) #f #t)))
(define (scmus-toggle-random!)
  (scmus-random-set! (if (scmus-random?) #f #t)))
(define (scmus-toggle-single!)
  (scmus-single-set! (if (scmus-single?) #f #t)))
(define (scmus-toggle-consume!)
  (scmus-consume-set! (if (scmus-consume?) #f #t)))

(define (scmus-search-songs exact add . constraints)
  (if (null? constraints)
    '()
    (if exact
      (if add
        (apply scmus-find-add! constraints)
        (apply scmus-find constraints))
      (if add
        (apply scmus-search-add! constraints)
        (apply scmus-search constraints)))))
