;;
;; Copyright 2014-2017 Drew Thoreson
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
         (uses event mpd-client option scmus-error track)
         (hide scmus-try-reconnect scmus-command))

(import scmus-base event mpd-client option scmus-error status track)

(: *last-update* number)
(define *last-update* -1.0)

(: scmus-connect! (string (or boolean fixnum) (or boolean string) -> boolean))
(define (scmus-connect! host port pass)
  (handle-exceptions e
    (begin (scmus-error-set! e) #f)
    (let ((con (mpd:connect host port pass)))
      (if (scmus-connected?)
        (mpd:disconnect (current-connection)))
      (set! (current-connection) con)
      (register-event! 'db-changed)
      #t)))

(: scmus-disconnect! thunk)
(define (scmus-disconnect!)
  (mpd:disconnect (current-connection)))

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
  (if (current-connection)
    (mpd:disconnect (current-connection))))

(: scmus-try-reconnect thunk)
(define (scmus-try-reconnect)
  (condition-case
    (begin (set! (current-connection) (mpd:reconnect (current-connection))) #t)
    (e () (scmus-error-set! e) #f)))

(: scmus-update-stats! thunk)
(define (scmus-update-stats!)
  (set! (current-stats) (scmus-stats)))

(: scmus-update-status! thunk)
(define (scmus-update-status!)
  (let* ((old-status (current-status))
         (new-status (scmus-status)))
    (when (and (alist-ref 'updating_db old-status)
               (not (alist-ref 'updating_db new-status)))
      (register-event! 'db-changed))
    (set! (current-status) new-status)
    (register-event! 'status-changed)
    (unless (= (alist-ref 'songid old-status eqv? -1)
               (alist-ref 'songid new-status eqv? -1))
      (register-event! 'track-changed))))

(: scmus-update-current-song! thunk)
(define (scmus-update-current-song!)
  (set! (current-track) (scmus-current-song))
  (register-event! 'queue-changed)
  (register-event! 'current-line-changed))

(: scmus-update-queue! thunk)
(define (scmus-update-queue!)
  (set! (current-queue) (scmus-playlist-info))
  (register-event! 'queue-data-changed))

;; Status update timer
(register-timer!
  (rec (scmus-update-client!)
    (let ((version (scmus-queue-version)))
      (when (scmus-connected?)
        (condition-case
          (begin
            (scmus-update-status!)
            (unless (= (scmus-song-id) (track-id (current-track)))
              (scmus-update-current-song!))
            (unless (= version (scmus-queue-version))
              (scmus-update-queue!)))
          (e () (scmus-error-set! e)
                (scmus-try-reconnect)))))
    (register-timer! scmus-update-client!
                     (get-option 'status-update-interval)))
  -1)

(: scmus-bail! (* -> null))
(define (scmus-bail! e)
  (scmus-disconnect!)
  (scmus-error-set! e)
  '())

(: *scmus-command ((mpd-connection #!rest * -> *) #!rest * -> list))
(define (*scmus-command mpd-fn . args)
  (if (scmus-connected?)
    (condition-case (apply mpd-fn (current-connection) args)
      (e () (scmus-error-set! e)
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
                       (max 0 (+ (car (scmus-elapsed-time)) seconds)))))

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
        (apply scmus-search constraints)))))
