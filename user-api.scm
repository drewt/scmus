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

(require-extension matchable)

(import drewt.ncurses
        scmus.base
        scmus.client
        scmus.command
        scmus.command-line
        scmus.config
        scmus.ueval
        scmus.event
        scmus.format
        scmus.keys
        scmus.option
        scmus.status
        scmus.track
        scmus.tui
        scmus.widgets)

;; Export an identifier unchanged (for SRFIs).
(define-syntax export-identifier!
  (syntax-rules ()
    ((export-identifier identifier)
      (user-export! (quote identifier) identifier))))

(define-syntax export/user
  (syntax-rules ()
    ((export/user name doc)
      (user-value-set! (quote name) name doc))))

(define-syntax define/user
  (syntax-rules ()
    ((define/user (name . args) doc first . rest)
      (user-value-set! (quote name) (lambda args first . rest) doc))
    ((define/user name doc value)
      (user-value-set! (quote name) value doc))))

(define-syntax define+user
  (syntax-rules ()
    ((define+user (name . args) doc first . rest)
      (begin (define (name . args) first . rest)
             (export/user name doc)))))

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

;; macros {{{

(define (user-syntax-error name arg-list)
  (abort (make-composite-condition
           (make-property-condition 'exn
                                    'message (format "Syntax error: in expansion of ~a" name)
                                    'arguments (cons name arg-list)
                                    'location name)
           (make-property-condition 'syntax))))

(user-macro-set! (string->symbol "\u03bb")
  (lambda (args) (cons 'lambda args)))

(user-macro-set! 'define-command
  (match-lambda
    ((((? symbol? name) . args) first . rest)
      `(register-command! (quote ,name)
         (lambda ,args ,first . ,rest)
         #t))
    (expr (user-syntax-error 'define-command expr))))

;; macros {{{
;; procedures {{{

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
  ; FIXME: *user-api* not exported (nor should it be)
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

(define+user (enter-eval-mode #!optional (text "") (cursor-pos 0))
  "Set the input mode to eval-mode"
  (command-line-get-string 'eval
    (lambda (s)
      (when s
        (let ((r (user-eval-string s)))
          (if (and (not (condition? r))
                   (not (eqv? r (void)))
                   (get-option 'eval-mode-print))
            (command-line-print-info! (format "~s" r))))))
    text
    cursor-pos))

(define/user (enter-command-mode #!optional (text "") (cursor-pos 0))
  "Set the input mode to command-mode"
  (command-line-get-string 'command
    (lambda (s)
      (when s (run-command s)))
    text
    cursor-pos))

(define/user (exit)
  "Exit the program"
  (scmus-exit 0))

(define/user get-environment-variable
  "Get the value of an environment variable"
  get-environment-variable)

(define/user get-option
  "Get the value of a configuration option"
  get-option)

(define/user (load file)
  "Load a Scheme file or command script"
  (cond
    ((string-suffix-ci? ".scm"  file) (user-load file))
    ((string-suffix-ci? ".scmd" file) (load-command-script file)))
  (void))

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

(define/user (play! #!optional (track-or-pos (current-track)))
  "Play the current track"
  (cond
    ((and (list? track-or-pos)
          (>= (track-id track-or-pos) 0))
      (scmus-play-id! (track-id track-or-pos)))
    ((integer? track-or-pos)
      (scmus-play-pos! track-or-pos)))
  (void))

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

(define/user (register-command! name handler #!optional force?)
  "Register a procedure to handle a command"
  (if (and (not force?)
           (command-exists? name))
    #f
    (begin (register-command! name handler) #t)))

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

(define+user (shell! command . args)
  "Run a shell command"
  (process-fork
    (lambda ()
      (handle-exceptions exn (void)
        (process-execute command args)))))

(define+user (shell-sync! command . args)
  "Run a shell command synchronously"
  (nth-value 2 (process-wait (apply shell! command args))))

(define/user (shell-term! command . args)
  "Run a shell command synchronously, with curses off"
  (without-curses
    (apply shell-sync! command args)))

;; Spawn a shell command, capturing STDOUT and STDERR as input ports.
;; XXX: the returned ports MUST be closed by the caller.
(define (async-shell! command . args)
  (let-values (((stdout-in stdout-out) (create-pipe))
               ((stderr-in stderr-out) (create-pipe)))
    (let ((child (process-fork
                   (lambda ()
                     (duplicate-fileno stdout-out fileno/stdout)
                     (duplicate-fileno stderr-out fileno/stderr)
                     (file-close stdout-out)
                     (file-close stderr-out)
                     (file-close stdout-in)
                     (file-close stderr-in)
                     (handle-exceptions exn (void)
                       (process-execute command args)))))
          (stdout-in-port (open-input-file* stdout-in))
          (stderr-in-port (open-input-file* stderr-in)))
      (file-close stdout-out)
      (file-close stderr-out)
      (values child stdout-in-port stderr-in-port))))

;; Wrapper for ASYNC-SHELL! that automatically closes pipes.
(define (call-with-shell proc command . args)
  (let-values (((pid stdout stderr) (apply async-shell! command args)))
    (let ((ret (proc pid stdout stderr)))
      (file-close (port->fileno stdout))
      (file-close (port->fileno stderr))
      ret)))

(define/user (shell!/capture-stdout command . args)
  "Run a shell command, returning the contents of standard output as a string"
  (apply call-with-shell
    (lambda (pid stdout stderr)
      (with-output-to-string
        (lambda ()
          (let loop ()
            (unless (eof-object? (peek-char stdout))
              (write-char (read-char stdout))
              (loop))))))
    command args))

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

(define/user (win-move! n #!optional relative)
  "Move the cursor up or down"
  (widget-move (widget-focus view-widget) n relative))

(define/user (win-bottom!)
  "Move the cursor to the bottom of the window"
  (widget-move-bottom (widget-focus view-widget)))

(define/user (win-top!)
  "Move the cursor to the top of the window"
  (widget-move-top (widget-focus view-widget)))

(define/user (win-activate!)
  "Activate the row at the cursor"
  (widget-activate (widget-focus view-widget)))

(define/user (win-deactivate!)
  "Deactivate the window"
  (widget-deactivate (widget-focus view-widget)))

(define/user (win-add!)
  "Add the selected row"
  (widget-add (widget-focus view-widget)))

(define/user (win-remove!)
  "Remove the selected row"
  (widget-remove (widget-focus view-widget)))

(define/user (win-clear!)
  "Clear the current window"
  (widget-clear (widget-focus view-widget)))

(define/user (win-move-tracks! #!optional (before #f))
  "Move the marked tracks to the cursor"
  (widget-paste (widget-focus view-widget) before))

(: enter-search-mode thunk)
(define/user (enter-search-mode #!optional (text "") (cursor-pos 0))
  "Set the input mode to search-mode"
  (command-line-get-string 'search
    (lambda (s)
      (when s
        (current-search-query s)
        (widget-search (widget-focus view-widget) s #f)))
    text
    cursor-pos))

(define+user (win-search! query)
  "Search the current window"
  (current-search-query query)
  (widget-search (widget-focus view-widget)
                 (current-search-query)
                 #f))

(define+user (win-search-next!)
  "Move the cursor to the next search result"
  (widget-search (widget-focus view-widget)
                 (current-search-query)
                 #f))

(define+user (win-search-prev!)
  "Move the cursor to the previous search result"
  (widget-search (widget-focus view-widget)
                 (current-search-query)
                 #t))

(define/user (win-edit!)
  "Edit the selected row"
  (widget-edit (widget-focus view-widget)))

(define/user (win-mark!)
  "Mark the selected row"
  (widget-mark (widget-focus view-widget)))

(define/user (win-unmark!)
  "Unmark the selected row"
  (widget-unmark (widget-focus view-widget)))

(define/user (win-toggle-mark!)
  "Toggle the marked status of the selected row"
  (widget-toggle-mark (widget-focus view-widget)))

(define/user win-clear-marked!
  "Unmark all marked rows"
  (widget-clear-marked (widget-focus view-widget)))

(define/user write-config!
  "Write the current configuration settings to a file"
  (return-void write-config!))

(define/user xfade
  "Get the value of the xfade setting"
  scmus-xfade)

;; SRFI-13 {{{

(export-identifier! string?)
(export-identifier! string-null?)
(export-identifier! string-every)
(export-identifier! string-any)
(export-identifier! make-string)
(export-identifier! string)
(export-identifier! string-tabulate)
(export-identifier! string->list)
(export-identifier! list->string)
(export-identifier! reverse-list->string)
(export-identifier! string-join)
(export-identifier! string-length)
(export-identifier! string-ref)
(export-identifier! string-copy)
(export-identifier! substring/shared)
(export-identifier! string-copy!)
(export-identifier! string-take)
(export-identifier! string-drop)
(export-identifier! string-take-right)
(export-identifier! string-drop-right)
(export-identifier! string-pad)
(export-identifier! string-pad-right)
(export-identifier! string-trim)
(export-identifier! string-trim-right)
(export-identifier! string-trim-both)
(export-identifier! string-set!)
(export-identifier! string-fill!)
(export-identifier! string-compare)
(export-identifier! string-compare-ci)
(export-identifier! string=)
(export-identifier! string<>)
(export-identifier! string<)
(export-identifier! string>)
(export-identifier! string<=)
(export-identifier! string>=)
(export-identifier! string-ci=)
(export-identifier! string-ci<>)
(export-identifier! string-ci<)
(export-identifier! string-ci>)
(export-identifier! string-ci<=)
(export-identifier! string-ci>=)
(export-identifier! string-hash)
(export-identifier! string-hash-ci)
(export-identifier! string-prefix-length)
(export-identifier! string-suffix-length)
(export-identifier! string-prefix-length-ci)
(export-identifier! string-suffix-length-ci)
(export-identifier! string-prefix?)
(export-identifier! string-suffix?)
(export-identifier! string-prefix-ci?)
(export-identifier! string-suffix-ci?)
(export-identifier! string-index)
(export-identifier! string-index-right)
(export-identifier! string-skip)
(export-identifier! string-skip-right)
(export-identifier! string-count)
(export-identifier! string-contains)
(export-identifier! string-contains-ci)
(export-identifier! string-titlecase)
(export-identifier! string-titlecase!)
(export-identifier! string-upcase)
(export-identifier! string-upcase!)
(export-identifier! string-downcase)
(export-identifier! string-downcase!)
(export-identifier! string-reverse)
(export-identifier! string-reverse!)
(export-identifier! string-append)
(export-identifier! string-concatenate)
(export-identifier! string-concatenate/shared)
(export-identifier! string-append/shared)
(export-identifier! string-concatenate-reverse)
(export-identifier! string-concatenate-reverse/shared)
(export-identifier! string-map)
(export-identifier! string-map!)
(export-identifier! string-fold)
(export-identifier! string-fold-right)
(export-identifier! string-unfold)
(export-identifier! string-unfold-right)
(export-identifier! string-for-each)
(export-identifier! string-for-each-index)
(export-identifier! xsubstring)
(export-identifier! string-xcopy!)
(export-identifier! string-replace)
(export-identifier! string-tokenize)
(export-identifier! string-filter)
(export-identifier! string-delete)
(export-identifier! string-parse-start+end)
(export-identifier! string-parse-final-start+end)
(export-identifier! check-substring-spec)
(export-identifier! substring-spec-ok?)
(export-identifier! make-kmp-restart-vector)
(export-identifier! kmp-step)
(export-identifier! string-kmp-partial-search)

;; SRFI-13 }}}
;; procedures }}}
