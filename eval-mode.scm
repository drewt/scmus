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

(require-extension sandbox)
 
(declare (unit eval-mode)
         (uses format keys ncurses option scmus-client ui-curses window)
         (export init-sandbox win-search! user-bind! user-eval user-eval-string
                 user-load))

;; user functions {{{

(define (user-format fmt #!optional (track '()) (len (- (COLS) 2)))
  (if (format-string-valid? fmt)
    (scmus-format (process-format fmt) len track)
    (abort
      (make-composite-condition
        (make-property-condition 'exn 'message "invalid format string"
                                 'arguments fmt)
        (make-property-condition 'scmus)))))

(define (user-bind! keys context expr #!optional (force #f))
  (let ((key-list (if (list? keys) keys (string-tokenize keys))))
    (if (binding-keys-valid? key-list)
      (begin
        (when force
          (unbind! key-list context))
        (make-binding! key-list context expr))
      #f)))

(define (user-unbind! keys context)
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (unbind! key-list context)
      #f)))

(define (echo! arg)
  (define (clean-text text)
    (string-delete (lambda (x)
                     (case x
                       ((#\newline #\linefeed) #t)
                       (else #f)))
                   text))
  (command-line-print-info! (clean-text (format #f "~a" arg)))
  arg)

(define (colorscheme! str)
  (cond
    ((file-exists? (format "~a/colors/~a.scm" *user-config-dir* str))
      => user-load)
    ((file-exists? (format "~a/colors/~a.scm" *scmus-dir* str))
      => user-load)))

(define (shell! command  . args)
  (process-fork
    (lambda ()
      (handle-exceptions exn (void)
        (process-execute command args)))))

;; Synchronous version of shell!
(define (shell-sync! command . args)
  (nth-value 2 (process-wait (apply shell! command args))))

(define (shell-term! command . args)
  (without-curses
    (apply shell-sync! command args)))

(define (update! #!optional (path #f))
  (scmus-update! path))

(define (rescan! #!optional (path #f))
  (scmus-rescan! path))

(define (set-volume! val #!optional (relative #f))
  (scmus-volume-set!
    (if relative
      (+ (scmus-volume) val)
      val)))

(: push! (string #!optional fixnum -> undefined))
(define (push! str #!optional index)
  (enter-eval-mode)
  (command-line-text-set! str)
  (when index
    (command-line-cursor-pos-set! index)))

(: win-move! (fixnum #!optional boolean -> undefined))
(define (win-move! nr #!optional (relative #f))
  (let ((nr-lines (if relative
                    (integer-scale (window-nr-lines (current-window)) nr)
                    nr)))
    (if (> nr-lines 0)
      (window-move-down! (current-window) nr-lines)
      (window-move-up! (current-window) (abs nr-lines)))))

(: win-bottom! thunk)
(define (win-bottom!)
  (let ((window (current-window)))
    (window-select! window (- (window-data-len window) 1))))

(: win-clear-marked! thunk)
(define (win-clear-marked!)
  (window-clear-marked! (current-window))
  (redraw-ui))

(: win-search! (string -> undefined))
(define (win-search! query)
  (window-search-init! (current-window) query)
  (win-search-next!))

(: win-search-next! thunk)
(define (win-search-next!)
  (let ((i (window-next-match! (current-window))))
    (when i
      (window-select! (current-window) i))))

(: win-search-prev! thunk)
(define (win-search-prev!)
  (let ((i (window-prev-match! (current-window))))
    (when i
      (window-select! (current-window) i))))

(: win-move-tracks! (#!optional boolean -> undefined))
(define (win-move-tracks! #!optional (before #f))
  (view-move! (current-view) before))

(define (describe symbol)
  (let ((info (alist-ref symbol user-api))
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

 ;; user functions }}}

(define *user-env* (make-safe-environment parent: default-safe-environment
                                          mutable: #t))

(define (user-export! name obj)
  (safe-environment-set! *user-env* name obj))

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

(define user-api
  `((bind!               ,user-bind!
                         "Bind a key sequence to a Scheme expression")
    (clear-queue!        ,(return-void scmus-clear!)
                         "Clear the queue")
    (colorscheme!        ,(return-void colorscheme!)
                         "Set the color scheme")
    (connect!            ,(lambda args (apply connect! args)) ; XXX: connect! not defined yet
                         "Connect to a MPD server")
    (consume?            ,scmus-consume?
                         "Check if MPD is in comsume mode")
    (consume-set!        ,(return-void scmus-consume-set!)
                         "Set consume mode on or off")
    (current-bitrate     ,scmus-bitrate
                         "Get the current bitrate of the current track")
    (current-elapsed     ,scmus-elapsed
                         "Get the elapsed time of the current track")
    (current-track       ,current-track
                         "Get the current track")
    (current-volume      ,scmus-volume
                         "Get the current volume")
    (describe            ,describe
                         "Describe a symbol")
    (disconnect!         ,(return-void scmus-disconnect!)
                         "Disconnect from the MPD server")
    (echo!               ,echo!
                         "Echo an expression on the command line")
    (get-option          ,get-option
                         "Get the value of a configuration option")
    (load                ,(return-void user-load)
                         "Load a Scheme file")
    (mixramp-db          ,scmus-mixrampdb
                         "Get the current mixramp-db value")
    (mixramp-delay       ,scmus-mixrampdelay
                         "Get the current mixramp-delay value")
    (mpd-address         ,scmus-address
                         "Get the address of the MPD server")
    (mpd-host            ,scmus-hostname
                         "Get the hostname of the MPD server")
    (mpd-port            ,scmus-port
                         "Get the port of the MPD server")
    (next!               ,(return-void scmus-next!)
                         "Play the next track in the queue")
    (next-id             ,scmus-next-song-id
                         "Get the ID of the next track in the queue")
    (next-pos            ,scmus-next-song
                         "Get the position of the next track in the queue")
    (pause!              ,(return-void scmus-toggle-pause!)
                         "Pause the current track")
    (play!               ,(return-void scmus-play!)
                         "Begin playing the current track")
    (playlist-clear!     ,(return-void scmus-playlist-clear!)
                         "Clear the given playlist")
    (playlist-add!       ,(return-void scmus-playlist-add!)
                         "Add a track to the given playlist")
    (playlist-move!      ,(return-void scmus-playlist-move!)
                         "Move a track in the given playlist")
    (playlist-delete!    ,(return-void scmus-playlist-delete!)
                         "Delete a track in the given playlist")
    (playlist-save!      ,(return-void scmus-playlist-save!)
                         "Save the current contents of the queue as a playlist")
    (playlist-load!      ,(return-void scmus-playlist-load!)
                         "Load the given playlist into the queue")
    (playlist-rename!    ,(return-void scmus-playlist-rename!)
                         "Rename the given playlist")
    (playlist-rm!        ,(return-void scmus-playlist-rm!)
                         "Delete the given playlist")
    (prev!               ,(return-void scmus-prev!)
                         "Play the previous track in the queue")
    (push!               ,(return-void push!)
                         "Push a string onto the command line")
    (queue-delete!       ,(return-void scmus-delete!)
                         "Delete a track from the queue")
    (queue-delete-id!    ,(return-void scmus-delete-id!)
                         "Delete a track from the queue by ID")
    (queue-length        ,scmus-queue-length
                         "Get the length of the queue")
    (queue-move!         ,(return-void scmus-move!)
                         "Move a track in the queue")
    (queue-move-id!      ,(return-void scmus-move-id!)
                         "Move a track in the queue by id")
    (queue-swap!         ,(return-void scmus-swap!)
                         "Swap tracks in the queue")
    (queue-swap-id!      ,(return-void scmus-swap-id!)
                         "Swap tracks in the queue by id")
    (queue-version       ,scmus-queue-version
                         "Get the queue version")
    (refresh-library!    ,(thunk (register-event! 'db-changed))
                         "Refresh the library view's data")
    (random?             ,scmus-random?
                         "Check if MPD is in random mode")
    (random-set!         ,(return-void scmus-random-set!)
                         "Set random mode on or off")
    (repeat?             ,scmus-repeat?
                         "Check if MPD is in repeat mode")
    (repeat-set!         ,(return-void scmus-repeat-set!)
                         "Set repeat mode on or off")
    (rescan!             ,(return-void rescan!)
                         "Rescan the music database")
    (scmus-format        ,user-format
                         "Generate formatted text")
    (seek!               ,(return-void scmus-seek!)
                         "Seek forwards or backwards in the current track")
    (set-option!         ,(return-void set-option!)
                         "Set the value of an option")
    (set-view!           ,(return-void set-view!)
                         "Change the current view")
    (set-volume!         ,(return-void set-volume!)
                         "Set the volume")
    (shell!              ,shell!
                         "Run a shell command")
    (shell-sync!         ,shell-sync!
                         "Run a shell command synchronously")
    (shell-term!         ,shell-term!
                         "Run a shell command synchronously, with curses off")
    (shuffle!            ,(return-void scmus-shuffle!)
                         "Shuffle the queue")
    (single?             ,scmus-single?
                         "Check if MPD is in single mode")
    (single-set!         ,(return-void scmus-single-set!)
                         "Set single mode on or off")
    (state               ,scmus-state
                         "Get the current player state")
    (stop!               ,(return-void scmus-stop!)
                         "Stop playing")
    (toggle-consume!     ,(return-void scmus-toggle-consume!)
                         "Toggle consume mode")
    (toggle-random!      ,(return-void scmus-toggle-random!)
                         "Toggle random mode")
    (toggle-repeat!      ,(return-void scmus-toggle-repeat!)
                         "Toggle repeat mode")
    (toggle-single!      ,(return-void scmus-toggle-single!)
                         "Toggle single mode")
    (track-album         ,track-album
                         "Get the album from a track object")
    (track-albumartist   ,track-albumartist
                         "Get the albumartist from a track object")
    (track-artist        ,track-artist
                         "Get the artist from a track object")
    (track-composer      ,track-composer
                         "Get the composer from a track object")
    (track-date          ,track-date
                         "Get the date from a track object")
    (track-disc          ,track-disc
                         "Get the disc from a track object")
    (track-duration      ,track-duration
                         "Get the duration from a track object")
    (track-end           ,track-end
                         "Get the end from a track object")
    (track-file          ,track-file
                         "Get the file from a track object")
    (track-genre         ,track-genre
                         "Get the genre from a track object")
    (track-id            ,track-id
                         "Get the ID from a track object")
    (track-last-modified ,track-last-modified
                         "Get the last-modified date from a track object")
    (track-name          ,track-name
                         "Get the name from a track object")
    (track-performer     ,track-performer
                         "Get the performer from a track object")
    (track-pos           ,track-pos
                         "Get the position from a track object")
    (track-prio          ,track-prio
                         "Get the prio from a track object")
    (track-start         ,track-start
                         "Get the start from a track object")
    (track-title         ,track-title
                         "Get the title from a track object")
    (track-track         ,track-track
                         "Get the track number from a track object")
    (unbind!             ,user-unbind!
                         "Unbind a key sequence")
    (update!             ,(return-void update!)
                         "Update the music database")
    (win-move!           ,(return-void win-move!)
                         "Move the cursor up or down")
    (win-bottom!         ,(return-void win-bottom!)
                         "Move the cursor to the bottom of the window")
    (win-top!            ,(thunk (window-select! (current-window) 0))
                         "Move the cursor to the top of the window")
    (win-activate!       ,(thunk (window-activate! (current-window)))
                         "Activate the row at the cursor")
    (win-deactivate!     ,(thunk (window-deactivate! (current-window)))
                         "Deactivate the window")
    (win-add!            ,(thunk (view-add! (current-view)))
                         "Add the selected row")
    (win-remove!         ,(thunk (view-remove! (current-view)))
                         "Remove the selected row")
    (win-clear!          ,(thunk (view-clear! (current-view)))
                         "Clear the current window")
    (win-move-tracks!    ,(return-void win-move-tracks!)
                         "Move the marked tracks to the cursor")
    (win-search!         ,(return-void win-search!)
                         "Search the current window")
    (win-search-next!    ,(return-void win-search-next!)
                         "Move the cursor to the next search result")
    (win-search-prev!    ,(return-void win-search-prev!)
                         "Move the cursor to the previous search result")
    (win-edit!           ,(thunk (view-edit! (current-view)))
                         "Edit the selected row")
    (win-sel-pos         ,(lambda () (window-sel-pos (current-window)))
                         "Get the position of the cursor")
    (win-selected        ,(lambda () (window-selected (current-window)))
                         "Get the selected row")
    (win-mark!           ,(thunk (window-mark! (current-window)))
                         "Mark the selected row")
    (win-unmark!         ,(thunk (window-unmark! (current-window)))
                         "Unmark the selected row")
    (win-toggle-mark!    ,(thunk (window-toggle-mark! (current-window)))
                         "Toggle the marked status of the selected row")
    (win-clear-marked!   ,(return-void win-clear-marked!)
                         "Unmark all marked rows")
    (*win-marked         ,(lambda () (*window-marked (current-window)))
                         "Get the marked rows, excluding the selected row")
    (win-marked          ,(lambda () (window-marked (current-window)))
                         "Get the marked rows, including the selected row")
    (write-config!       ,(return-void write-config!)
                         "Write the current configuration settings to a file")
    (xfade               ,scmus-xfade
                         "Get the value of the xfade setting")))

(define (init-sandbox)
  (safe-environment-macro-set! *user-env* (string->symbol "\u03bb")
    (lambda (args)
      (cons 'lambda args)))
  (for-each (lambda (info)
              (user-export! (car info) (cadr info)))
            user-api))

(: user-eval procedure)
(define (user-eval expr)
  (condition-case (safe-eval expr environment: *user-env*)
    (e () (error-set! e) e)))

(: user-eval-string (string -> *))
(define (user-eval-string str)
  (condition-case (safe-eval (with-input-from-string str read)
                             environment: *user-env*)
    (e () (error-set! e) e)))

(: user-load (string -> *))
(define (user-load path)
  (call-with-input-file path
    (lambda (in)
      (let loop ()
       (let ((input (read in)))
         (unless (eqv? input #!eof)
           (condition-case (safe-eval input environment: *user-env*)
             (e () (error-set! e)))
           (loop)))))))
