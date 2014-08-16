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

(require-extension regex srfi-1 tcp)

(declare (unit mpd-client)
         (hide make-connection mpd-host-set! mpd-port-set! in-port in-port-set!
               out-port out-port-set! mpd-version-set! re-ok+version
               re-err re-pair raise-mpd-error check-connection send-command
               playlist-is-number? convert-type read-response parse-songs
               flatten-constraints format-range range-or-number))

(define-record-type mpd-connection
  (make-connection hostname port in-port out-port version)
  mpd-connection?
  (hostname mpd-host mpd-host-set!)
  (port mpd-port mpd-port-set!)
  (in-port in-port in-port-set!)
  (out-port out-port out-port-set!)
  (version mpd-version mpd-version-set!))

(define re-ok+version (regexp "^OK MPD (.*)$"))
(define re-err (regexp "^ACK ?(.*)$"))
(define re-pair (regexp "^([^:]+): (.*)$"))

(define (raise-mpd-error msg . args)
  (abort
    (make-composite-condition
      (make-property-condition 'exn 'message msg 'arguments args)
      (make-property-condition 'mpd))))

(define (mpd:connect #!optional (hostname "localhost") (port 6600))
  (mpd:reconnect (make-connection hostname port #f #f #f)))

(define (mpd:disconnect con)
  (close-input-port (in-port con))
  (close-output-port (out-port con))
  (in-port-set! con #f)
  (in-port-set! con #f)
  (void))

(define (mpd:reconnect con)
  (if (in-port con)
    (mpd:disconnect con))
  (let-values (((in out) (tcp-connect (mpd-host con) (mpd-port con))))
    (let ((l (read-line in)))
      (cond
        ((eof-object? l)
          (close-input-port in)
          (close-output-port out)
          (raise-mpd-error "connection closed unexpectedly"))
        ((string-search re-ok+version l)
          => (lambda (m)
               (in-port-set! con in)
               (out-port-set! con out)
               (mpd-version-set! con (cadr m))
               con))
        (else
          (close-input-port in)
          (close-output-port out)
          (raise-mpd-error "unexpected greeting" l))))))

(define (check-connection con)
  (let ((in (in-port con)))
    (if (and (char-ready? in)
             (eof-object? (peek-char in)))
      (mpd:reconnect con))))

(define (process-arg arg)
  (format " ~s" (format "~a" arg)))

(define (send-command con cmd . args)
  (check-connection con)
  (let ((line (fold (lambda (x a) (string-append a (process-arg x)))
                    cmd
                    args)))
    (write-line line (out-port con))))

(define (mpd:send-command con cmd . args)
  (apply send-command con cmd args)
  (read-response-for con cmd))

(define playlist-is-number? (make-parameter #f))

(define (convert-type k v)
  (case k
    ((volume playlistlength song songid bitrate xfade id pos elapsed
             artists albums songs uptime playtime db_playtime db_update
             updating_db outputid cpos nextsong nextsongid mixrampdb)
      (string->number v))
    ((playlist)
      (if (playlist-is-number?)
        (string->number v)
        v))
    ((time audio)
      (map string->number (string-split v ":")))
    ((repeat random single consume outputenabled)
      (not (string=? v "0")))
    ((state)
      (string->symbol v))
    (else v)))

(define (read-response con)
  (let loop ((l (read-line (in-port con))) (pairs '()))
    (cond
      ((eof-object? l)
        (mpd:disconnect con)
        (raise-mpd-error "connection closed unexpectedly"))
      ((equal? l "OK")
        (reverse pairs))
      ((string-search re-err l)
        => (lambda (m)
             (raise-mpd-error "error from server" (cadr m))))
      ((string-search re-pair l)
        => (lambda (m)
             (let ((s (string->symbol (string-downcase (cadr m)))))
               (loop (read-line (in-port con))
                     (cons (cons s (convert-type s (caddr m)))
                           pairs)))))
      (else
        (raise-mpd-error "unexpected line from server" l)))))

(define (parse-songs pairs)
  (let loop ((pairs pairs) (cur #f) (songs '()))
    (define (next-songs)
      (if cur (cons (reverse cur) songs) songs))
    (cond
      ((null? pairs)
        (reverse (next-songs)))
      ((eqv? (caar pairs) 'file)
        (loop (cdr pairs) (cons (car pairs) '()) (next-songs)))
      ((not cur)
        (raise-mpd-error "unexpected pair" (car pairs)))
      (else
        (loop (cdr pairs) (cons (car pairs) cur) songs)))))

(define (read-response-for con cmd)
  (case (string->symbol cmd)
    ((listplaylist listplaylistinfo find findadd search searchadd)
      (parse-songs (read-response con)))
    (else (read-response con))))

(define (flatten-constraints constraints)
  (reverse (fold (lambda (x a)
                   (cons (cdr x) (cons (symbol->string (car x)) a)))
                 '()
                 constraints)))

(define (format-range range)
  (format "~a:~a" (car range) (cdr range)))

(define (range-or-number arg)
  (if (pair? arg)
    (format-range arg)
    arg))

(define-syntax define-simple-command
  (syntax-rules (0 1 2 3 4)
    ((define-simple-command 0 name cmd)
      (define (name con)
        (send-command con cmd)
        (read-response con)))
    ((define-simple-command 1 name cmd)
      (define (name con arg0)
        (send-command con cmd arg0)
        (read-response con)))
    ((define-simple-command 2 name cmd)
      (define (name con arg0 arg1)
        (send-command con cmd arg0 arg1)
        (read-response con)))
    ((define-simple-command 3 name cmd)
      (define (name con arg0 arg1 arg2)
        (send-command con cmd arg0 arg1 arg2)
        (read-response con)))
    ((define-simple-command 4 name cmd)
      (define (name con arg0 arg1 arg2 arg3)
        (send-command con cmd arg0 arg1 arg2 arg3)
        (read-response con)))))

(define-syntax define-optional-command
  (syntax-rules ()
    ((define-optional-command name cmd)
      (define (name con #!optional (arg #f))
        (if arg
          (send-command con cmd arg)
          (send-command con cmd))
        (read-response con)))))

(define-syntax define-boolean-setter
  (syntax-rules ()
    ((define-boolean-setter name cmd)
      (define (name con val)
        (send-command con cmd (if val 1 0))
        (read-response con)))))

(define-syntax define-constraint-command
  (syntax-rules ()
    ((define-constraint-command name cmd)
      (define (name con first . rest)
        (apply send-command con cmd (flatten-constraints (cons first rest)))
        (read-response con)))))

(define-syntax define-songs-wrapper
  (syntax-rules ()
    ((define-songs-wrapper name fun)
      (define (name con . args)
        (parse-songs (apply fun con args))))))

;; Querying MPD's status
(define-simple-command 0 mpd:clear-error! "clearerror")
(define-simple-command 0 mpd:current-song "currentsong")
(define (mpd:status con)
  (parameterize ((playlist-is-number? #t))
    (send-command con "status")
    (read-response con)))
(define-simple-command 0 mpd:stats "stats")

;; Playback options
(define-boolean-setter   mpd:consume-set! "consume")
(define-boolean-setter   mpd:random-set! "random")
(define-boolean-setter   mpd:repeat-set! "repeat")
(define-boolean-setter   mpd:single-set! "single")
(define-simple-command 1 mpd:crossfade-set! "crossfade")
(define-simple-command 1 mpd:mixrampdb-set! "mixrampdb")
(define-simple-command 1 mpd:mixrampdelay-set! "mixrampdelay")
(define-simple-command 1 mpd:volume-set! "setvol")
(define-simple-command 1 mpd:replay-gain-mode-set! "replay_gain_mode")
(define-simple-command 0 mpd:replay-gain-status "replay_gain_status")

;; Controlling playback
(define-simple-command 0 mpd:next! "next")
(define-boolean-setter   mpd:pause! "pause")
(define-simple-command 0 mpd:toggle-pause! "pause")
(define-simple-command 0 mpd:play! "play")
(define-simple-command 1 mpd:play-pos! "play")
(define-simple-command 1 mpd:play-id! "playid")
(define-simple-command 0 mpd:previous! "previous")
(define-simple-command 2 mpd:seek! "seek")
(define-simple-command 2 mpd:seek-id! "seekid")
(define-simple-command 1 mpd:seek-cur! "seekcur")
(define-simple-command 0 mpd:stop! "stop")

;; The current playlist
(define-simple-command 1 mpd:add! "add")
(define-simple-command 1 mpd:add-id! "addid")
(define-simple-command 2 mpd:add-id-at! "addid")
(define-simple-command 0 mpd:clear! "clear")
(define (mpd:delete! con arg)
  (send-command con "delete" (range-or-number arg))
  (read-response con))
(define-simple-command 1 mpd:delete-id! "deleteid")
(define (mpd:move! con from to)
  (send-command con "delete" (range-or-number from) to)
  (read-response con))
(define-simple-command 2 mpd:move-id! "moveid")
(define-simple-command 2 mpd:playlist-find "playlistfind")
(define-simple-command 1 mpd:playlist-id "playlistid")
(define (mpd:playlist-info con #!optional (arg #f))
  (if arg
    (send-command con "playlistinfo" (range-or-number arg))
    (send-command con "playlistinfo"))
  (parse-songs (read-response con)))
(define-simple-command 2 mpd:playlist-search "playlistsearch")
(define-simple-command 1 mpd:playlist-changes "plchanges")
(define-simple-command 1 mpd:playlist-changes-posid "plchangesposid")
(define (mpd:prio-set! con prio first . rest)
  (apply send-command con "prio" (map format-range (cons first rest)))
  (read-response con))
(define (mpd:prio-id-set! con prio first . rest)
  (apply send-command con "prioid" (cons first rest))
  (read-response con))
(define (mpd:shuffle! con #!optional (range #f))
  (if range
    (send-command con "shuffle" (format-range range))
    (send-command con "shuffle"))
  (read-response con))
(define-simple-command 2 mpd:swap! "swap")
(define-simple-command 2 mpd:swap-id! "swapid")
(define-simple-command 3 mpd:add-tag-id! "addtagid")
(define (mpd:clear-tag-id! con songid #!optional (tag #f))
  (if tag
    (send-command con "cleartagid" songid tag)
    (send-command con "cleartagid" songid))
  (read-response con))

;; Stored playlists
(define-simple-command 1 *mpd:list-playlist "listplaylist")
(define-songs-wrapper mpd:list-playlist *mpd:list-playlist)
(define-simple-command 1 *mpd:list-playlist-info "listplaylistinfo")
(define-songs-wrapper mpd:list-playlist-info *mpd:list-playlist-info)
(define-simple-command 0 mpd:list-playlists "listplaylists")
(define (mpd:playlist-load! con name #!optional (range #f))
  (if range
    (send-command con "load" name (format-range range))
    (send-command con "load" name))
  (read-response con))
(define-simple-command 2 mpd:playlist-add! "playlistadd")
(define-simple-command 1 mpd:playlist-clear! "playlistclear")
(define-simple-command 2 mpd:playlist-delete! "playlistdelete")
(define-simple-command 3 mpd:playlist-move! "playlistmove")
(define-simple-command 2 mpd:playlist-rename! "rename")
(define-simple-command 1 mpd:playlist-rm! "rm")
(define-simple-command 1 mpd:playlist-save! "save")

;; The music database
(define-simple-command 2   mpd:count "count")
(define-constraint-command *mpd:find "find")
(define-songs-wrapper      mpd:find *mpd:find)
(define-constraint-command *mpd:find-add! "findadd")
(define-songs-wrapper      mpd:find-add! *mpd:find-add!)
(define (mpd:list-tags con type . rest)
  (apply send-command con "list" type (flatten-constraints rest))
  (read-response con))
(define-optional-command   mpd:list-all "listall")
(define-optional-command   mpd:list-all-info "listallinfo")
(define-optional-command   mpd:list-files "listfiles")
(define-optional-command   mpd:lsinfo "lsinfo")
(define-simple-command 1   mpd:read-comments "readcomments")
(define-constraint-command *mpd:search "search")
(define-songs-wrapper      mpd:search *mpd:search)
(define-constraint-command *mpd:search-add! "searchadd")
(define-songs-wrapper      mpd:search-add! *mpd:search-add!)
(define (*mpd:search-add-pl! con name first . rest)
  (apply send-command con "searchaddpl" name (flatten-constraints (cons first rest))))
(define-songs-wrapper      mpd:search-add-pl! *mpd:search-add-pl!)
(define-optional-command   mpd:update! "update")
(define-optional-command   mpd:rescan! "rescan")

;; Stickers
(define-simple-command 3 mpd:sticker-get "sticker get")
(define-simple-command 4 mpd:sticker-set! "sticker set")
(define-simple-command 2 mpd:sticker-delete! "sticker delete")
(define-simple-command 2 mpd:sticker-delete-all! "sticker delete")
(define-simple-command 2 mpd:sticker-list "sticker list")
(define-simple-command 3 mpd:sticker-find "sticker find")

;; Connection settings
(define-simple-command 0 mpd:close! "close")
(define-simple-command 0 mpd:kill! "kill")
(define-simple-command 1 mpd:password "password")
(define-simple-command 0 mpd:ping "ping")

;; Audio output devices
(define-simple-command 1 mpd:disable-output! "disableoutput")
(define-simple-command 1 mpd:enable-output! "enableoutput")
(define-simple-command 1 mpd:toggle-output! "toggleoutput")
(define-simple-command 0 mpd:list-outputs "outputs")

;; Reflection
(define-simple-command 0 mpd:config "config")
(define-simple-command 0 mpd:commands "commands")
(define-simple-command 0 mpd:not-commands "notcommands")
(define-simple-command 0 mpd:tag-types "tagtypes")
(define-simple-command 0 mpd:url-handlers "urlhandlers")
(define-simple-command 0 mpd:decoders "decoders")

;; Client to client
(define-simple-command 1 mpd:subscribe! "subscribe")
(define-simple-command 1 mpd:unsubscribe! "unsubscribe")
(define-simple-command 0 mpd:channels "channels")
(define-simple-command 0 mpd:read-messages "readmessages")
(define-simple-command 2 mpd:send-message! "sendmessage")
