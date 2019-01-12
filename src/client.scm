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

(module scmus.client (scmus-connect!
                      scmus-disconnect!
                      scmus-oneshot
                      exit-client
                      scmus-update-stats!
                      scmus-update-status!
                      scmus-update-current-song!
                      scmus-update-queue!
                      scmus-clear-error!
                      scmus-current-song
                      scmus-status
                      scmus-stats
                      scmus-consume-set!
                      scmus-random-set!
                      scmus-repeat-set!
                      scmus-single-set!
                      scmus-crossfade-set!
                      scmus-mixrampdb-set!
                      scmus-mixrampdelay-set!
                      scmus-volume-set!
                      scmus-replay-gain-mode-set!
                      scmus-replay-gain-status
                      scmus-next!
                      scmus-pause!
                      scmus-toggle-pause!
                      scmus-play!
                      scmus-play-pos!
                      scmus-play-id!
                      scmus-prev!
                      scmus-seek-pos!
                      scmus-seek-id!
                      scmus-seek-cur!
                      scmus-stop!
                      scmus-add!
                      scmus-add-id!
                      scmus-add-id-at!
                      scmus-clear!
                      scmus-delete!
                      scmus-delete-id!
                      scmus-move!
                      scmus-move-id!
                      scmus-playlist-info
                      scmus-shuffle!
                      scmus-swap!
                      scmus-swap-id!
                      scmus-list-playlist
                      *scmus-list-playlists
                      scmus-playlist-load!
                      scmus-playlist-add!
                      scmus-playlist-clear!
                      scmus-playlist-delete!
                      scmus-playlist-move!
                      scmus-playlist-rename!
                      scmus-playlist-rm!
                      scmus-playlist-save!
                      scmus-count
                      scmus-find
                      scmus-find-add!
                      scmus-list-tags
                      scmus-list-all
                      scmus-list-all-info
                      scmus-list-files
                      scmus-lsinfo
                      scmus-read-comments
                      scmus-search
                      scmus-search-add!
                      scmus-search-add-pl!
                      scmus-update!
                      scmus-rescan!
                      scmus-list-playlists
                      scmus-play-track!
                      scmus-seek!
                      scmus-toggle-repeat!
                      scmus-repeat-set!
                      scmus-toggle-random!
                      scmus-random-set!
                      scmus-toggle-single!
                      scmus-single-set!
                      scmus-toggle-consume!
                      scmus-consume-set!
                      scmus-search-songs)
  (import drewt.mpd-client
          scmus.base
          scmus.error
          scmus.event
          scmus.option
          scmus.status
          scmus.track)

  (: *last-update* number)
  (define *last-update* -1.0)

  (define (handle-mpd-error e)
    (let ((errno (get-condition-property e 'mpd 'errno #f)))
      (when (and errno (or (= errno 3)
                           (= errno 4)))
        (signal-event/global 'mpd-unauthenticated)))
    (scmus-error e))

  (: scmus-connect! (string (or boolean fixnum) (or boolean string) -> boolean))
  (define (scmus-connect! host port pass)
    (define (do-connect)
      (let ((con (mpd:connect host port pass)))
        (if (scmus-connected?)
          (mpd:disconnect (current-connection)))
        (current-connection con)
        (signal-event/global 'db-changed)
        #t))
    (condition-case (do-connect)
      (e (mpd) (handle-mpd-error e))
      (e ()    (scmus-error e))))

  (: scmus-disconnect! thunk)
  (define (scmus-disconnect!)
    (when (scmus-connected?)
      (mpd:disconnect (current-connection))
      (current-queue  '())
      (current-status '())
      (current-stats  '())
      (current-track  '())
      (signal-event/global 'db-changed)
      (signal-event/global 'queue-data-changed)
      (signal-event/global 'status-changed)
      (signal-event/global 'track-changed)))

  (: scmus-oneshot ((or boolean string)
                    (or boolean fixnum)
                    (or boolean string)
                    string
                    #!rest *
                    -> *))
  (define (scmus-oneshot host port pass cmd . args)
    (condition-case
      (let* ((con (mpd:connect (if host host (get-option 'mpd-address))
                               (if port port (get-option 'mpd-port))
                               (if pass pass (get-option 'mpd-password))))
             (res (apply mpd:send-command con cmd args)))
        (mpd:disconnect con)
        res)
      (e () (condition->list e))))

  (: exit-client thunk)
  (define (exit-client)
    (if (scmus-connected?)
      (mpd:disconnect (current-connection))))

  (: scmus-update-stats! thunk)
  (define (scmus-update-stats!)
    (current-stats (scmus-stats)))

  (: scmus-update-status! thunk)
  (define (scmus-update-status!)
    (let* ((old-status (current-status))
           (new-status (scmus-status)))
      (when (and (alist-ref 'updating_db old-status)
                 (not (alist-ref 'updating_db new-status)))
        (signal-event/global 'db-changed))
      (current-status new-status)
      (signal-event/global 'status-changed)
      (unless (= (alist-ref 'songid old-status eqv? -1)
                 (alist-ref 'songid new-status eqv? -1))
        (signal-event/global 'track-changed)))
    (set! *last-update* (current-second)))

  (: scmus-update-current-song! thunk)
  (define (scmus-update-current-song!)
    (current-track (scmus-current-song))
    (signal-event/global 'queue-changed)
    (signal-event/global 'track-changed))

  (: scmus-update-queue! thunk)
  (define (scmus-update-queue!)
    (current-queue (scmus-playlist-info))
    (signal-event/global 'queue-data-changed))

  (: do-full-update thunk)
  (define (do-full-update)
    (let ((version (scmus-queue-version)))
      (scmus-update-status!)
      (unless (= (scmus-song-id) (track-id (current-track)))
        (scmus-update-current-song!))
      (unless (= version (scmus-queue-version))
        (scmus-update-queue!))))

  ;; Status update timer.  Ticks every 0.5 seconds.  Does a full status update
  ;; every STATUS-UPDATE-INTERVAL seconds, otherwise just updates elapsed time.
  (register-timer!
    (rec (scmus-update-client!)
      (when (scmus-connected?)
        (let ((now (current-second)))
          (if (>= (- now *last-update*)
                 (get-option 'status-update-interval))
            (do-full-update)
            (when (eqv? (scmus-state) 'play)
              (scmus-elapsed-tick! 0.5)
              (signal-event/global 'status-changed)))))
      (register-timer! scmus-update-client! 0.5))
    -1)

  (: *scmus-command ((mpd-connection #!rest * -> *) #!rest * -> list))
  (define (*scmus-command mpd-fn . args)
    (define (try-reconnect)
      (handle-exceptions e (begin (scmus-error e) #f)
        (current-connection (mpd:reconnect (current-connection)))
        #t))
    (if (scmus-connected?)
      (condition-case (apply mpd-fn (current-connection) args)
        (e (mpd) (handle-mpd-error e) '())
        (e ()
          (scmus-error e)
          (unless (try-reconnect)
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

  (: scmus-list-playlists (-> list))
  (define (scmus-list-playlists)
    (filter (lambda (x) (eqv? (car x) 'playlist))
            (*scmus-list-playlists)))

  (: scmus-play-track! (track -> undefined))
  (define (scmus-play-track! track)
    (assert (>= (track-id track) 0) "scmus-play-track!" (track-id track))
    (scmus-play-id! (track-id track)))

  (: scmus-seek! (fixnum -> undefined))
  (define (scmus-seek! seconds)
    (assert (integer? seconds) "scmus-seek!" seconds)
    (scmus-seek-id! (track-id (current-track))
                    (min (track-duration (current-track))
                         (max 0 (+ (scmus-elapsed) seconds)))))

  (: scmus-toggle-repeat! thunk)
  (define (scmus-toggle-repeat!)
    (scmus-repeat-set! (if (scmus-repeat?) #f #t)))
  (: scmus-toggle-random! thunk)
  (define (scmus-toggle-random!)
    (scmus-random-set! (if (scmus-random?) #f #t)))
  (: scmus-toggle-single! thunk)
  (define (scmus-toggle-single!)
    (scmus-single-set! (if (scmus-single?) #f #t)))
  (: scmus-toggle-consume thunk)
  (define (scmus-toggle-consume!)
    (scmus-consume-set! (if (scmus-consume?) #f #t)))

  (: scmus-searech-songs (boolean boolean #!rest (pair symbol *) -> list))
  (define (scmus-search-songs exact add . constraints)
    (if (null? constraints)
      '()
      (if exact
        (if add
          (apply scmus-find-add! constraints)
          (apply scmus-find constraints))
        (if add
          (apply scmus-search-add! constraints)
          (apply scmus-search constraints))))))
