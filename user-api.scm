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

(declare (unit user-api)
         (uses command-line config eval-mode event format keys ncurses option
               scmus-client status track ui-curses window))

(import scmus-base command-line config eval-mode event format ncurses option
        status track window)

(define-syntax define/user
  (syntax-rules ()
    ((define/user (name . args) doc first . rest)
      (register-user-value! (quote name) (lambda args first . rest) doc))
    ((define/user name doc value)
      (register-user-value! (quote name) value doc))))

(define-syntax export/user
  (syntax-rules ()
    ((export/user name doc)
      (register-user-value! (quote name) name doc))))

;; More succinct syntax for defining thunks, + ensuring that they return void.
(define-syntax thunk
  (syntax-rules ()
    ((thunk body ...)
       (lambda () body ... (void)))))

;; Syntax for creating a function wrapper which discards the return value.
(define-syntax return-void
  (syntax-rules ()
    ((return-void fun)
       (lambda args (apply fun args) (void)))))

(: *user-events* (list-of symbol))
(define *user-events*
  '(track-changed))

(define/user (bind! keys context expr #!optional (force #f))
  "Bind a key sequence to a Scheme expression"
  (let ((key-list (if (list? keys) keys (string-tokenize keys))))
    (if (binding-keys-valid? key-list)
      (begin
        (when force
          (unbind! key-list context))
        (make-binding! key-list context expr))
      #f)))

(define/user clear-queue!
  "Clear the queue"
  (return-void scmus-clear!))

(define/user (colorscheme! str)
  "Set the color scheme"
  (cond
    ((file-exists? (format "~a/colors/~a.scm" *user-config-dir* str))
      => user-load)
    ((file-exists? (format "~a/colors/~a.scm" *scmus-dir* str))
      => user-load)))

(define/user connect!
  "Connect to an MPD server"
  ; XXX: connect! not defined yet
  (lambda args (apply connect! args)))

(define/user consume?
  "Check if MPD is in consume mode"
  scmus-consume?)

(define/user consume-set!
  "Set consume mode on or off"
  (return-void scmus-consume-set!))

(define/user current-bitrate
  "Get the current bitrate of the current track"
  scmus-bitrate)

(define/user current-elapsed
  "Get the elapsed time of the current track"
  scmus-elapsed)

(define/user (current-track)
  "Get the current track"
  (current-track))

(define/user current-volume
  "Get the current volume"
  scmus-volume)

(define/user (describe symbol)
  "Describe a symbol"
  (let ((info (alist-ref symbol *user-api*))
        (bind (safe-environment-ref *user-env* symbol)))
    (command-line-print-info!
      (cond
        ((not (symbol? symbol))
           (format "~s" symbol))
        ((not bind)
           (format "~a: unbound" symbol))
        ((or (not info)
             (not (eqv? (car info) bind)))
           (format "~a: ~s" symbol bind))
        (else
           (format "~a: ~a" symbol (cadr info)))))))

(define/user disconnect!
  "Disconnect from the MPD server"
  (return-void scmus-disconnect!))

(define/user (echo! arg)
  "Echo a value on the command line"
  (define (clean-text text)
    (string-delete (lambda (x)
                     (case x
                       ((#\newline #\linefeed) #t)
                       (else #f)))
                   text))
  (command-line-print-info! (clean-text (format #f "~a" arg)))
  arg)

(define/user get-option
  "Get the value of a configuration option"
  get-option)

(define/user load
  "Load a Scheme file"
  (return-void user-load))

(define/user mixramp-db
  "Get the current mixramp-db value"
  scmus-mixrampdb)

(define/user mixramp-delay
  "Get the current mixramp-delay value"
  scmus-mixrampdelay)

(define/user mpd-address
  "Get the address of the MPD server"
  scmus-address)

(define/user mpd-host
  "Get the hostname of the MPD server"
  scmus-hostname)

(define/user mpd-port
  "Get the port of the MPD server"
  scmus-port)

(define/user next!
  "Play the next track in the queue"
  (return-void scmus-next!))

(define/user next-id
  "Get the ID of the next track in the queue"
  scmus-next-song-id)

(define/user next-pos
  "Get the position of the next track in the queue"
  scmus-next-song)

(define/user pause!
  "Pause the current track"
  (return-void scmus-toggle-pause!))

(define/user play!
  "Play the current track"
  (return-void scmus-play!))

(define/user playlist-clear!
  "Clear the given playlist"
  (return-void scmus-playlist-clear!))

(define/user playlist-add!
  "Add a track to the given playlist"
  (return-void scmus-playlist-add!))

(define/user playlist-move!
  "Move a track in the given playlist"
  (return-void scmus-playlist-move!))

(define/user playlist-delete!
  "Delete a track in the given playlist"
  (return-void scmus-playlist-delete!))

(define/user playlist-save!
  "Save the current contents of the queue as a playlist"
  (return-void scmus-playlist-save!))

(define/user playlist-load!
  "Load the given playlist into the queue"
  (return-void scmus-playlist-load!))

(define/user playlist-rename!
  "Rename the given playlist"
  (return-void scmus-playlist-rename!))

(define/user playlist-rm!
  "Delete the given playlist"
  (return-void scmus-playlist-rm!))

(define/user prev!
  "Play the previous track in the queue"
  (return-void scmus-prev!))

(define/user (push! str #!optional index)
  "Push a string onto the command line"
  (enter-eval-mode)
  (command-line-text-set! str)
  (when index
    (command-line-cursor-pos-set! index)))

(define/user queue-delete!
  "Delete a track from the queue"
  (return-void scmus-delete!))

(define/user queue-delete-id!
  "Delete a track from the queue by ID"
  (return-void scmus-delete-id!))

(define/user queue-length
  "Get the length of the queue"
  scmus-queue-length)

(define/user queue-move!
  "Move a track in the queue"
  (return-void scmus-move!))

(define/user queue-move-id!
  "Move a track in the queue by ID"
  (return-void scmus-move-id!))

(define/user queue-swap!
  "Swap tracks in the queue"
  (return-void scmus-swap!))

(define/user queue-swap-id!
  "Swap tracks in the queue by ID"
  (return-void scmus-swap-id!))

(define/user queue-version
  "Get the queue version"
  scmus-queue-version)

(define/user random?
  "Check if MPD is in random mode"
  scmus-random?)

(define/user random-set!
  "Set random mode on or off"
  (return-void scmus-random-set!))

(define/user refresh-library!
  "Refresh the library view's data"
  (thunk (register-event! 'db-changed)))

(define/user (register-event-handler! event handler)
  "Register an event handler"
  (when (member event *user-events*)
    (register-event-handler! event handler)) 
  (void))

(define/user repeat?
  "Check if MPD is in repeat mode"
  scmus-repeat?)

(define/user repeat-set!
  "Set repeat mode on or off"
  (return-void scmus-repeat-set!))

(define/user (rescan! #!optional (path #f))
  "Rescan the music database"
  (scmus-rescan!)
  (void))

(define/user (scmus-format fmt #!optional (track '()) (len (- (COLS) 2)))
  "Generate formatted text"
  (if (format-string-valid? fmt)
    (scmus-format (process-format fmt) len track)
    (abort
      (make-composite-condition
        (make-property-condition 'exn 'message "invalid format string"
                                 'arguments fmt)
        (make-property-condition 'scmus)))))

(define/user seek!
  "Seek forwards or backwards in the current track"
  (return-void scmus-seek!))

(define/user set-option!
  "Set the value of an option"
  (return-void set-option!))

(define/user set-view!
  "Change the current view"
  (return-void set-view!))

(define/user set-volume!
  "Set the volume"
  (return-void set-volume!))

(define/user (shell! command . args)
  "Run a shell command"
  (process-fork
    (lambda ()
      (handle-exceptions exn (void)
        (process-execute command args)))))

(define/user (shell-sync! command . args)
  "Run a shell command synchronously"
  (nth-value 2 (process-wait (apply shell! command args))))

(define/user (shell-term! command . args)
  "Run a shell command synchronously, with curses off"
  (without-curses
    (apply shell-sync! command args)))

(define/user shuffle!
  "Shuffle the queue"
  (return-void scmus-shuffle!))

(define/user single?
  "Check if MPD is in single mode"
  scmus-single?)

(define/user single-set!
  "Set single mode on or off"
  (return-void scmus-single-set!))

(define/user (start-timer! thunk seconds #!key (recurring #f))
  "Start a timer to run a thunk after a number of seconds"
  (let ((event (gensym)))
    (register-event-handler! event thunk)
    (register-timer-event! event seconds recurring: recurring)))

(define/user state
  "Get the current player state"
  scmus-state)

(define/user stop!
  "Stop playing"
  (return-void scmus-stop!))

(define/user toggle-consume!
  "Toggle consume mode"
  (return-void scmus-toggle-consume!))

(define/user toggle-random!
  "Toggle random mode"
  (return-void scmus-toggle-random!))

(define/user toggle-repeat!
  "Toggle repeat mode"
  (return-void scmus-toggle-repeat!))

(define/user toggle-single!
  "Toggle single mode"
  (return-void scmus-toggle-single!))

(define/user track-album
  "Get the album from a track object"
  track-album)

(define/user track-albumartist
  "Get the albumartist from a track object"
  track-albumartist)

(define/user track-artist
  "Get the artist from a track object"
  track-artist)

(define/user track-composer
  "Get the composer from a track object"
  track-composer)

(define/user track-date
  "Get the date from a track object"
  track-date)

(define/user track-disc 
  "Get the disc from a track object"
  track-disc)

(define/user track-duration
  "Get the duration from a track object"
  track-duration)

(define/user track-end
  "Get the end from a track object"
  track-end)

(define/user track-file
  "Get the file from a track object"
  track-file)

(define/user track-genre
  "Get the genre from a track object"
  track-genre)

(define/user track-id
  "Get the ID from a track object"
  track-id)

(define/user track-last-modified
  "Get the last-modified date from a track object"
  track-last-modified)

(define/user track-name
  "Get the name from a track object"
  track-name)

(define/user track-performer
  "Get the performer from a track object"
  track-performer)

(define/user track-pos
  "Get the position from a track object"
  track-pos)

(define/user track-prio
  "Get the prio from a track object"
  track-prio)

(define/user track-start
  "Get the start from a track object"
  track-start)

(define/user track-title
  "Get the title from a track object"
  track-title)

(define/user track-track
  "Get the track number from a track object"
  track-track)

(define/user (unbind! keys context)
  "Unbind a key sequence"
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (unbind! key-list context)
      #f)))

(define/user (update! #!optional (path #f))
  "Update the music database"
  (scmus-update! path)
  (void))

(define/user (win-move! nr #!optional (relative #f))
  "Move the cursor up or down"
  (let ((nr-lines (if relative
                    (integer-scale (window-nr-lines (current-window)) nr)
                    nr)))
    (if (> nr-lines 0)
      (window-move-down! (current-window) nr-lines)
      (window-move-up! (current-window) (abs nr-lines))))
  (void))

(: win-bottom! thunk)
(define/user (win-bottom!)
  "Move the cursor to the bottom of the window"
  (let ((window (current-window)))
    (window-select! window (- (window-data-len window) 1)))
  (void))

(define/user win-top!
  "Move the cursor to the top of the window"
  (thunk (window-select! (current-window) 0)))

(define/user win-activate!
  "Activate the row at the cursor"
  (thunk (window-activate! (current-window))))

(define/user win-deactivate!
  "Deactivate the window"
  (thunk (window-deactivate! (current-window))))

(define/user win-add!
  "Add the selected row"
  (thunk (view-add! (current-view))))

(define/user win-remove!
  "Remove the selected row"
  (thunk (view-remove! (current-view))))

(define/user win-clear!
  "Clear the current window"
  (thunk (view-clear! (current-view))))

(define/user (win-move-tracks! #!optional (before #f))
  "Move the marked tracks to the cursor"
  (view-move! (current-view) before))

(define/user win-next!
  "Switch to the next window in the current view"
  (thunk (view-next! (current-view))))

(define/user win-prev!
  "Switch to the previous window in the current view"
  (thunk (view-prev! (current-view))))

;; TODO: search-related stuff should go somewhere else
(define (win-search! query)
  (window-search-init! (current-window) query)
  (win-search-next!)
  (void))

(define (win-search-next!)
  (let ((i (window-next-match! (current-window))))
    (when i
      (window-select! (current-window) i)))
  (void))

(define (win-search-prev!)
  (let ((i (window-prev-match! (current-window))))
    (when i
      (window-select! (current-window) i)))
  (void))

(: enter-search-mode thunk)
(define (enter-search-mode)
  (command-line-get-string 'search
    (lambda (s)
      (when s (win-search! s)))))

(export/user win-search!      "Search the current window")
(export/user win-search-next! "Move the cursor to the next search result")
(export/user win-search-prev! "Move the cursor to the previous search result")

(define/user win-edit!
  "Edit the selected row"
  (thunk (view-edit! (current-view))))

(define/user win-sel-pos
  "Get the position of the cursor"
  (lambda () (window-sel-pos (current-window))))

(define/user win-selected
  "Get the selected row"
  (lambda () (window-selected (current-window))))

(define/user win-mark!
  "Mark the selected row"
  (thunk (window-mark! (current-window))))

(define/user win-unmark!
  "Unmark the selected row"
  (thunk (window-unmark! (current-window))))

(define/user win-toggle-mark!
  "Toggle the marked status of the selected row"
  (thunk (window-toggle-mark! (current-window))))

(define/user win-clear-marked!
  "Unmark all marked rows"
  (return-void win-clear-marked!))

(define/user *win-marked
  "Get the marked rows, excluding the selected row"
  (lambda () (*window-marked (current-window))))

(define/user win-marked
  "Get the marked rows, including the selected row"
  (lambda () (window-marked (current-window))))

(define/user write-config!
  "Write the current configuration settings to a file"
  (return-void write-config!))

(define/user xfade
  "Get the value of the xfade setting"
  scmus-xfade)
