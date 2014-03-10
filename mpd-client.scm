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

(declare (unit mpd-client)
         (export mpd:connect mpd:disconnect mpd:reconnect mpd:error-code
                 mpd:error-message mpd:get-stats mpd:get-status
                 mpd:get-current-song mpd:list-queue mpd:db-list-tags
                 mpd:db-search-songs mpd:play! mpd:play-id! mpd:play-pos!
                 mpd:pause! mpd:stop! mpd:next-song! mpd:previous-song!
                 mpd:seek-id! mpd:seek-pos! mpd:repeat-set! mpd:random-set!
                 mpd:single-set! mpd:consume-set! mpd:add! mpd:add-id!
                 mpd:add-id-to! mpd:delete! mpd:delete-id! mpd:delete-range!
                 mpd:shuffle! mpd:shuffle-range! mpd:clear! mpd:move!
                 mpd:move-range! mpd:swap! mpd:swap-id!))

(foreign-declare "#include <mpd/client.h>")
(include "libmpdclient.scm")

(define (mpd:connect host port #!optional (timeout 5000))
  (mpd_connection_new host port timeout))

(define mpd:disconnect mpd_connection_free)

(define (mpd:reconnect connection)
  (let* ((settings (mpd_connection_get_settings connection))
         (host (mpd_settings_get_host settings))
         (port (mpd_settings_get_port settings)))
    (mpd_connection_free connection)
    (mpd:connect host port)))

(define mpd:error-code
  (condition-property-accessor 'mpd 'code))
(define mpd:error-message
  (condition-property-accessor 'mpd 'message))

(define (mpd:raise-error connection)
  (abort (make-property-condition
           'mpd
           'code (mpd_connection_get_error connection)
           'message (mpd_connection_get_error_message connection))))

(define (mpd:get-stats connection)
  (let* ((stats (mpd_run_stats connection))
         (error (mpd_connection_get_error connection))
         (retval
           (if stats
             (list
               (cons 'artists (mpd_stats_get_number_of_artists stats))
               (cons 'albums (mpd_stats_get_number_of_albums stats))
               (cons 'songs (mpd_stats_get_number_of_songs stats))
               (cons 'uptime (mpd_stats_get_uptime stats))
               (cons 'db-update (mpd_stats_get_db_update_time stats))
               (cons 'playtime (mpd_stats_get_play_time stats))
               (cons 'db-playtime (mpd_stats_get_db_play_time stats)))
             '())))
     (if stats
       (mpd_stats_free stats))
     (unless (= error MPD_ERROR_SUCCESS)
       (mpd:raise-error connection))
     retval))

(define (mpd-state->symbol state)
  (cond
    ((= state MPD_STATE_UNKNOWN) 'unknown)
    ((= state MPD_STATE_STOP) 'stop)
    ((= state MPD_STATE_PLAY) 'play)
    ((= state MPD_STATE_PAUSE) 'pause)))

(define (symbol->mpd-tag tag)
  (case tag
    ((artist)      MPD_TAG_ARTIST)
    ((album)       MPD_TAG_ALBUM)
    ((albumartist) MPD_TAG_ALBUM_ARTIST)
    ((title)       MPD_TAG_TITLE)
    ((tracknumber) MPD_TAG_TRACK)
    ((name)        MPD_TAG_NAME)
    ((genre)       MPD_TAG_GENRE)
    ((date)        MPD_TAG_DATE)
    ((composer)    MPD_TAG_COMPOSER)
    ((performer)   MPD_TAG_PERFORMER)
    ((comment)     MPD_TAG_COMMENT)
    ((discnumber)  MPD_TAG_DISC)))

(define (mpd:get-status connection)
  (let* ((status (mpd_run_status connection))
         (error (mpd_connection_get_error connection))
         (retval
           (if status
             (list
               (cons 'volume (mpd_status_get_volume status))
               (cons 'repeat (mpd_status_get_repeat status))
               (cons 'random (mpd_status_get_random status))
               (cons 'single (mpd_status_get_single status))
               (cons 'consume (mpd_status_get_consume status))
               (cons 'queue-length (mpd_status_get_queue_length status))
               (cons 'queue-version (mpd_status_get_queue_version status))
               (cons 'state (mpd-state->symbol
                              (mpd_status_get_state status)))
               (cons 'xfade (mpd_status_get_crossfade status))
               (cons 'mixrampdb (mpd_status_get_mixrampdb status))
               (cons 'mixrampdelay (mpd_status_get_mixrampdelay status))
               (cons 'song-pos (mpd_status_get_song_pos status))
               (cons 'song-id (mpd_status_get_song_id status))
               (cons 'next-song-pos (mpd_status_get_next_song_pos status))
               (cons 'next-song-id (mpd_status_get_next_song_id status))
               (cons 'elapsed-time (mpd_status_get_elapsed_time status))
               (cons 'elapsed-ms (mpd_status_get_elapsed_ms status))
               (cons 'total-time (mpd_status_get_total_time status))
               (cons 'bitrate (mpd_status_get_kbit_rate status))
               ; TODO: audio format
               (cons 'updating (= 1 (mpd_status_get_update_id status)))
               (cons 'error (mpd_status_get_error status)))
             '())))
    (if status
      (mpd_status_free status))
    (if (= error MPD_ERROR_SUCCESS)
      retval
      (mpd:raise-error connection))))

(define (song->alist song)
  (list
    (cons 'file (mpd_song_get_uri song))
    (cons 'artist (mpd_song_get_tag song MPD_TAG_ARTIST 0))
    (cons 'album (mpd_song_get_tag song MPD_TAG_ALBUM 0))
    (cons 'albumartist
          (mpd_song_get_tag song MPD_TAG_ALBUM_ARTIST 0))
    (cons 'title (mpd_song_get_tag song MPD_TAG_TITLE 0))
    (cons 'track (mpd_song_get_tag song MPD_TAG_TRACK 0))
    (cons 'name (mpd_song_get_tag song MPD_TAG_NAME 0))
    (cons 'genre (mpd_song_get_tag song MPD_TAG_GENRE 0))
    (cons 'date (mpd_song_get_tag song MPD_TAG_DATE 0))
    (cons 'composer (mpd_song_get_tag song MPD_TAG_COMPOSER 0))
    (cons 'performer (mpd_song_get_tag song MPD_TAG_PERFORMER 0))
    (cons 'comment (mpd_song_get_tag song MPD_TAG_COMMENT 0))
    (cons 'disc (mpd_song_get_tag song MPD_TAG_DISC 0))
    (cons 'duration (mpd_song_get_duration song))
    (cons 'start (mpd_song_get_start song))
    (cons 'end (mpd_song_get_end song))
    (cons 'last-modified (mpd_song_get_last_modified song))
    (cons 'pos (mpd_song_get_pos song))
    (cons 'id (mpd_song_get_id song))
    (cons 'prio (mpd_song_get_prio song))))

(define (mpd-pair->string pair)
  (mpd_pair-value pair))

(define (mpd:get-current-song connection)
  (let* ((song (mpd_run_current_song connection))
         (error (mpd_connection_get_error connection))
         (retval (if song (song->alist song) '())))
    (if song
      (mpd_song_free song))
    (if (= error MPD_ERROR_SUCCESS)
      retval
      (mpd:raise-error connection))))

(define (read-songs connection lst)
  (let* ((song (mpd_recv_song connection))
         (error (mpd_connection_get_error connection))
         (next (if song (song->alist song) '())))
    (if song
      (begin
        (mpd_song_free song)
        (read-songs connection (cons next lst)))
      ; mpd_recv_song returned NULL
      (if (= error MPD_ERROR_SUCCESS)
        lst
        (mpd:raise-error connection)))))

(define (mpd:list-queue connection)
  (if (mpd_send_list_queue_meta connection)
    (reverse (read-songs connection '()))
    (mpd:raise-error connection)))

(define (mpd:list-queue-range connection start end)
  (if (mpd_send_list_queue_range connection start end)
    (reverse (read-songs connection '()))
    (mpd:raise-error connection)))

(define (read-tag-pairs connection tag lst)
  (let* ((pair (mpd_recv_pair_tag connection tag))
         (error (mpd_connection_get_error connection))
         (next (if pair (mpd-pair->string pair) '())))
    (if pair
      (begin
        (mpd_return_pair connection pair)
        (read-tag-pairs connection tag (if (not (string=? next ""))
                                         (cons next lst)
                                         lst)))
      ; mpd_recv_pair_tag returned NULL
      (if (= error MPD_ERROR_SUCCESS)
        lst
        (mpd:raise-error connection)))))

(define (mpd:search-add-tag-constraints connection constraints)
  (if (null? constraints)
    #t
    (if (mpd_search_add_tag_constraint connection
                                       MPD_OPERATOR_DEFAULT
                                       (symbol->mpd-tag (caar constraints))
                                       (cdar constraints))
      (mpd:search-add-tag-constraints connection (cdr constraints))
      #f)))

(define (mpd:db-list-tags connection tag . constraints)
  (let ((mpd-tag (symbol->mpd-tag tag)))
    (if (mpd_search_db_tags connection mpd-tag)
      (if (and (mpd:search-add-tag-constraints connection constraints)
               (mpd_search_commit connection))
        (reverse (read-tag-pairs connection mpd-tag '()))
        (begin (mpd_search_cancel connection)
               (mpd:raise-error connection)))
      (mpd:raise-error connection))))

(define (mpd:db-search-songs connection exact add . constraints)
  (let ((search (if add mpd_search_add_db_songs mpd_search_db_songs)))
    (if (search connection exact)
      (if (and (mpd:search-add-tag-constraints connection constraints)
               (mpd_search_commit connection))
        (reverse (read-songs connection '()))
        (begin (mpd_search_cancel connection)
               (mpd:raise-error connection)))
      (mpd:raise-error connection))))

(define-syntax mpd:define-wrapper
  (syntax-rules (0 1 2 3)
    ((mpd:define-wrapper 0 name mpd-fn)
      (define (name connection)
        (if (not (mpd-fn connection))
          (mpd:raise-error connection))))
    ((mpd:define-wrapper 1 name mpd-fn)
      (define (name connection arg)
        (if (not (mpd-fn connection arg))
          (mpd:raise-error connection))))
    ((mpd:define-wrapper 2 name mpd-fn)
      (define (name connection arg1 arg2)
        (if (not (mpd-fn connection arg1 arg2))
          (mpd:raise-error connection))))
    ((mpd:define-wrapper 3 name mpd-fn)
      (define (name connection arg1 arg2 arg3)
        (if (not (mpd-fn connection arg1 arg2 arg3))
          (mpd:raise-error connection))))))

(mpd:define-wrapper 0 mpd:play! mpd_run_play)
(mpd:define-wrapper 0 mpd:pause! mpd_run_toggle_pause)
(mpd:define-wrapper 0 mpd:stop! mpd_run_stop)
(mpd:define-wrapper 0 mpd:next-song! mpd_run_next)
(mpd:define-wrapper 0 mpd:previous-song! mpd_run_previous)
(mpd:define-wrapper 1 mpd:play-id! mpd_run_play_id)
(mpd:define-wrapper 1 mpd:play-pos! mpd_run_play_pos)
(mpd:define-wrapper 2 mpd:seek-id! mpd_run_seek_id)
(mpd:define-wrapper 2 mpd:seek-pos! mpd_run_seek_pos)
(mpd:define-wrapper 1 mpd:repeat-set! mpd_run_repeat)
(mpd:define-wrapper 1 mpd:random-set! mpd_run_random)
(mpd:define-wrapper 1 mpd:single-set! mpd_run_single)
(mpd:define-wrapper 1 mpd:consume-set! mpd_run_consume)
(mpd:define-wrapper 1 mpd:add! mpd_run_add)
(mpd:define-wrapper 1 mpd:delete! mpd_run_delete)
(mpd:define-wrapper 1 mpd:delete-id! mpd_run_delete_id)
(mpd:define-wrapper 2 mpd:delete-range! mpd_run_delete_range)
(mpd:define-wrapper 0 mpd:shuffle! mpd_run_shuffle)
(mpd:define-wrapper 2 mpd:shuffle-range! mpd_run_shuffle_range)
(mpd:define-wrapper 0 mpd:clear! mpd_run_clear)
(mpd:define-wrapper 2 mpd:move! mpd_run_move)
(mpd:define-wrapper 2 mpd:move-id! mpd_run_move_id)
(mpd:define-wrapper 3 mpd:move-range! mpd_run_move_range)
(mpd:define-wrapper 2 mpd:swap! mpd_run_swap)
(mpd:define-wrapper 2 mpd:swap-id! mpd_run_swap_id)

(define (mpd:add-id! connection file)
  (let ((rv (mpd_run_add_id connection file)))
    (if (= rv -1)
      (mpd:raise-error connection)
      rv)))

(define (mpd:add-id-to! connection file pos)
  (let ((rv (mpd_run_add_id_to connection file pos)))
    (if (= rv -1)
      (mpd:raise-error connection)
      rv)))
