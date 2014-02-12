;; Copyright (c) 2006-2007, Hans Bulfone
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;     * Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the author nor the names of his contributors may
;;       be used to endorse or promote products derived from this software
;;       without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; mpd-client.scm
;; client library for mpd (http://www.musicpd.org)

(declare (unit mpd-client))

(require-extension tcp regex srfi-1)

(import scheme chicken)
(import (only data-structures string-translate string-split)
        (only extras read-line)
        (only ports with-output-to-string)
        (only regex regexp string-search)
        (only tcp tcp-connect)
        (only srfi-1 filter-map))

(define-record-type :mpd-conn
  (make-mpd-conn host port password i o version time)
  mpd:connection?
  (host mpd:host)
  (port mpd:port)
  (password mpd:password)
  (i in-port in-port-set!)
  (o out-port out-port-set!)
  (version mpd:version mpd:version-set!)
  (time last-contact-time last-contact-time-set!))

(define-record-type :by-position
  (mpd:by-position pos)
  by-position?
  (pos position))

(define (update-time conn)
  (last-contact-time-set! conn (current-seconds)))

(define re-ok+version (regexp "^OK MPD (.*)$"))
(define re-err (regexp "^ACK ?(.*)$"))
(define re-pair (regexp "^([^:]+): (.*)$"))

(define (raise-mpd-error msg . args)
  (abort
   (make-composite-condition
    (make-property-condition 'exn 'message msg 'arguments args)
    (make-property-condition 'mpd))))

(define (mpd:connect #!optional (host "localhost") (port 6600) password)
  (reconnect (make-mpd-conn host port password #f #f #f 0)))

(define (reconnect conn)
  (if (in-port conn)
      (mpd:disconnect conn))
  (receive (i o) (tcp-connect (mpd:host conn) (mpd:port conn))
    (let ((l (read-line i)))
      (cond
       ((eof-object? l)
        (close-input-port i)
        (close-output-port o)
        (raise-mpd-error "connection closed unexpectedly"))
       ((string-search re-ok+version l)
        => (lambda (m)
             (in-port-set! conn i)
             (out-port-set! conn o)
             (mpd:version-set! conn (cadr m))
             (update-time conn)
             (cond ((mpd:password conn) => (cut mpd:cmd conn "password" <>)))
             conn))
       (else
        (close-input-port i)
        (close-output-port o)
        (raise-mpd-error "unexpected greeting" l))))))

(define (mpd:disconnect conn)
  (close-input-port (in-port conn))
  (close-output-port (out-port conn))
  (in-port-set! conn #f)
  (out-port-set! conn #f)
  (last-contact-time-set! conn 0)
  (void))

(define (mpd:ping conn)
  (mpd:send-command conn "ping" '())
  (let ((l (read-line (in-port conn))))
    (cond
     ((eof-object? l)
      (reconnect conn))
     ((equal? l "OK")
      (update-time conn))
     (else
      (update-time conn)
      (raise-mpd-error "unexpected line from server" l)))))

(define (mpd:check-connection conn)
  (let ((i (in-port conn)))
    (cond
     ((and (char-ready? i)
           (eof-object? (peek-char i)))
      (reconnect conn))
     ((> (current-seconds)
         (+ (last-contact-time conn) 30))
      (mpd:ping conn)))))

(define (mpd:get-result conn)
  (let loop ((l (read-line (in-port conn))) (r '()))
    (cond
     ((eof-object? l)
      (mpd:disconnect conn)
      (raise-mpd-error "connection closed unexpectedly"))
     ((equal? l "OK")
      (update-time conn)
      (reverse r))
     ((string-search re-err l)
      => (lambda (m)
           (update-time conn)
           (raise-mpd-error "error from server" (cadr m))))
     ((string-search re-pair l)
      => (lambda (m)
           (let ((s (string->symbol (cadr m))))
             (loop (read-line (in-port conn))
                   (cons (cons s (mpd:convert-type s (caddr m)))
                         r)))))
     (else
      (update-time conn)
      (raise-mpd-error "unexpected line from server" l)))))

(define mpd:playlist-is-number (make-parameter #f))

(define (mpd:convert-type k v)
  (case k
    ((volume playlistlength song songid bitrate xfade
             Id Pos Time Track
             artists albums songs uptime playtime db_playtime db_update
             updating_db outputid cpos)
     (string->number v))
    ((playlist)
     (if (mpd:playlist-is-number)
         (string->number v)
         v))
    ((time audio)
     (map string->number (string-split v ":")))
    ((repeat random outputenabled)
     (not (string=? v "0")))
    ((state)
     (string->symbol v))
    (else v)))

(define (mpd:send-command conn cmd args)
  (display
   (with-output-to-string
     (lambda ()
       (display cmd)
       (for-each
        (lambda (arg)
          (when arg
            (display " ")
            (display
             (cond
              ((string? arg)
               (string-append "\"" (string-translate arg "\"") "\""))
              (else arg)))))
        args)
       (newline)))
   (out-port conn))
  (update-time conn))

(define (mpd:cmd conn cmd . args)
  (mpd:check-connection conn)
  (mpd:send-command conn cmd args)
  (mpd:get-result conn))

(define (mpd:result/1-col colname result)
  (filter-map
   (lambda (p)
     (and (eqv? colname (car p)) (cdr p)))
   result))

(define (mpd:result/1-col* result)
  (map cdr result))

(define (mpd:result/m-col result)
  (let loop ((result result) (out '()))
    (cond
     ((null? result)
      (reverse out))
     ((char-lower-case? (string-ref (symbol->string (caar result)) 0))
      (loop (cdr result) (cons (list (car result)) out)))
     (else
      (loop (cdr result) (cons (cons (car result) (car out)) (cdr out)))))))

(define (mpd:result/m-col* result)
  (let loop ((result result) (out '()) (first-key #f))
    (cond
     ((null? result)
      (reverse out))
     ((not first-key)
      (loop result out (caar result)))
     ((eq? first-key (caar result))
      (loop (cdr result) (cons (list (car result)) out) first-key))
     (else
      (loop (cdr result) (cons (cons (car result) (car out)) (cdr out))
            first-key)))))


;; server information and status

(define (mpd:get-commands c #!optional (allowed #t))
  (mpd:result/1-col 'command (mpd:cmd c (if allowed "commands" "notcommands"))))
(define (mpd:get-stats c) (mpd:cmd c "stats"))
(define (mpd:get-status c)
  (parameterize ((mpd:playlist-is-number #t))
    (mpd:cmd c "status")))
(define (mpd:clear-error! c) (mpd:cmd c "clearerror"))
(define (mpd:shutdown-server! c) (mpd:cmd c "kill"))
(define (mpd:get-output-devices c) (mpd:result/m-col* (mpd:cmd c "outputs")))
(define (mpd:enable-output-device! c id) (mpd:cmd c "enableoutput" id))
(define (mpd:disable-output-device! c id) (mpd:cmd c "disableoutput" id))
(define (mpd:set-options! c . opts)
  (when (pair? opts)
    (case (car opts)
      ((#:crossfade) (mpd:cmd c "crossfade" (cadr opts)))
      ((#:random)    (mpd:cmd c "random" (if (cadr opts) 1 0)))
      ((#:repeat)    (mpd:cmd c "repeat" (if (cadr opts) 1 0)))
      ((#:volume)    (mpd:cmd c "setvol" (cadr opts)))
      (else          (raise-mpd-error "unknown option" (car opts))))
    (apply mpd:set-options! c (cddr opts))))

;; query and modify playlist

(define (mpd:add-song! c path) (mpd:result/1-col 'Id (mpd:cmd c "addid" path)))
(define (mpd:move-song! c from to)
  (if (by-position? from)
      (mpd:cmd c "move" (position from) to)
      (mpd:cmd c "moveid" from to)))
(define (mpd:remove-song! c song)
  (if (by-position? song)
      (mpd:cmd c "delete" (position song))
      (mpd:cmd c "deleteid" song)))
(define (mpd:swap-songs! c song1 song2)
  (cond
   ((and (by-position? song1) (by-position? song2))
    (mpd:cmd c "swap" (position song1) (position song2)))
   ((and (integer? song1) (integer? song2))
    (mpd:cmd c "swapid" song1 song2))
   (else (raise-mpd-error "both songs to be swapped must be specified in the same way"))))
(define (mpd:shuffle-playlist! c) (mpd:cmd c "shuffle"))
(define (mpd:clear-playlist! c) (mpd:cmd c "clear"))
(define (mpd:load-playlist! c pl) (mpd:cmd c "load" pl))
(define (mpd:save-playlist! c n) (mpd:cmd c "save" n))
(define (mpd:remove-playlist! c n) (mpd:cmd c "rm" n))
(define (mpd:get-current-song c) (mpd:cmd c "currentsong"))
(define (mpd:get-playlist c #!optional song)
  (mpd:result/m-col
   (if (by-position? song)
       (mpd:cmd c "playlistinfo" (position song))
       (mpd:cmd c "playlistid" song))))
(define (mpd:get-playlist-changes c version #!optional (full? #t))
  (mpd:result/m-col (mpd:cmd c (if full? "plchanges" "plchangesposid") version)))

;; song database

(define (mpd:find-songs c type s) (mpd:result/m-col (mpd:cmd c "find" type s)))
(define (mpd:search-songs c type s) (mpd:result/m-col (mpd:cmd c "search" type s)))
(define (mpd:list-metadata c type #!optional limit s)
  (mpd:result/1-col* (mpd:cmd c "list" type limit s)))
(define (mpd:list-directory c #!optional dir)
  (mpd:result/m-col (mpd:cmd c "lsinfo" dir)))
(define (mpd:list-directory/r c #!optional path (full? #t))
  (if full?
      (mpd:result/m-col (mpd:cmd c "listallinfo" path))
      (mpd:cmd c "listall" path)))
(define (mpd:update-song-database! c #!optional path)
  (mpd:cmd c "update" path))

;; playback control

(define (mpd:play! c #!optional song time)
  (let ((pos? (by-position? song)))
    (mpd:cmd c (cond ((and pos? time) "seek")
                 (time "seekid")
                 (pos? "play")
                 (else "playid"))
         (if pos? (position song) song)
         time)))
(define (mpd:pause! c pause?) (mpd:cmd c "pause" (if pause? 1 0)))
(define (mpd:stop! c) (mpd:cmd c "stop"))
(define (mpd:next-song! c) (mpd:cmd c "next"))
(define (mpd:previous-song! c) (mpd:cmd c "previous"))
