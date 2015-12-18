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
         (export init-sandbox user-bind! user-eval user-eval-string user-load))

;; user functions {{{

(define (user-color->char color)
  (cond
    ((*->color-code color) => (lambda (ch) (color->char ch)))
    (else #f)))

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

(define (init-sandbox)
  (safe-environment-macro-set! *user-env* (string->symbol "\u03bb")
    (lambda (args)
      (cons 'lambda args)))
  (user-export! 'string-length string-length)
  (user-export! 'string-ref string-ref)
  (user-export! 'string-set! string-set!)
  (user-export! 'make-string make-string)
  (user-export! 'string string)
  (user-export! 'substring substring)
  (user-export! 'string->list string->list)
  (user-export! 'list->string list->string)
  (user-export! 'string-fill! string-fill!)
  (user-export! 'bind! user-bind!)
  (user-export! 'clear-queue! (return-void scmus-clear!))
  (user-export! 'color->char user-color->char)
  (user-export! 'colorscheme! (return-void colorscheme!))
  (user-export! 'connect! scmus-connect!)
  (user-export! 'consume? scmus-consume?)
  (user-export! 'consume-set! (return-void scmus-consume-set!))
  (user-export! 'current-bitrate scmus-bitrate)
  (user-export! 'current-elapsed scmus-elapsed)
  (user-export! 'current-track current-track)
  (user-export! 'current-volume scmus-volume)
  (user-export! 'disconnect! (return-void scmus-disconnect!))
  (user-export! 'echo! echo!)
  (user-export! 'get-option get-option)
  (user-export! 'load (return-void user-load))
  (user-export! 'mixramp-db scmus-mixrampdb)
  (user-export! 'mixramp-delay scmus-mixrampdelay)
  (user-export! 'mpd-address scmus-address)
  (user-export! 'mpd-host scmus-hostname)
  (user-export! 'mpd-port scmus-port)
  (user-export! 'next! (return-void scmus-next!))
  (user-export! 'next-id scmus-next-song-id)
  (user-export! 'next-pos scmus-next-song)
  (user-export! 'pause! (return-void scmus-toggle-pause!))
  (user-export! 'play! (return-void scmus-play!))
  (user-export! 'playlist-clear! (return-void scmus-playlist-clear!))
  (user-export! 'playlist-add! (return-void scmus-playlist-add!))
  (user-export! 'playlist-move! (return-void scmus-playlist-move!))
  (user-export! 'playlist-delete! (return-void scmus-playlist-delete!))
  (user-export! 'playlist-save! (return-void scmus-playlist-save!))
  (user-export! 'playlist-load! (return-void scmus-playlist-load!))
  (user-export! 'playlist-rename! (return-void scmus-playlist-rename!))
  (user-export! 'playlist-rm! (return-void scmus-playlist-rm!))
  (user-export! 'prev! (return-void scmus-prev!))
  (user-export! 'push! (return-void push!))
  (user-export! 'queue-delete! (return-void scmus-delete!))
  (user-export! 'queue-delete-id! (return-void scmus-delete-id!))
  (user-export! 'queue-length scmus-queue-length)
  (user-export! 'queue-move! (return-void scmus-move!))
  (user-export! 'queue-move-id! (return-void scmus-move-id!))
  (user-export! 'queue-swap! (return-void scmus-swap!))
  (user-export! 'queue-swap-id! (return-void scmus-swap-id!))
  (user-export! 'queue-version scmus-queue-version)
  (user-export! 'refresh-library! (thunk (register-event! 'db-changed)))
  (user-export! 'random? scmus-random?)
  (user-export! 'random-set! (return-void scmus-random-set!))
  (user-export! 'repeat? scmus-repeat?)
  (user-export! 'repeat-set! (return-void scmus-repeat-set!))
  (user-export! 'rescan! (return-void rescan!))
  (user-export! 'scmus-format user-format)
  (user-export! 'seek! (return-void scmus-seek!))
  (user-export! 'set-option! (return-void set-option!))
  (user-export! 'set-view! (return-void set-view!))
  (user-export! 'set-volume! (return-void set-volume!))
  (user-export! 'shell! shell!)
  (user-export! 'shell-sync! shell-sync!)
  (user-export! 'shell-term! shell-term!)
  (user-export! 'shuffle! (return-void scmus-shuffle!))
  (user-export! 'single? scmus-single?)
  (user-export! 'single-set! (return-void scmus-single-set!))
  (user-export! 'state scmus-state)
  (user-export! 'stop! (return-void scmus-stop!))
  (user-export! 'toggle-consume! (return-void scmus-toggle-consume!))
  (user-export! 'toggle-random! (return-void scmus-toggle-random!))
  (user-export! 'toggle-repeat! (return-void scmus-toggle-repeat!))
  (user-export! 'toggle-single! (return-void scmus-toggle-single!))
  (user-export! 'track-album track-album)
  (user-export! 'track-albumartist track-albumartist)
  (user-export! 'track-artist track-artist)
  (user-export! 'track-composer track-composer)
  (user-export! 'track-date track-date)
  (user-export! 'track-disc track-disc)
  (user-export! 'track-duration track-duration)
  (user-export! 'track-end track-end)
  (user-export! 'track-file track-file)
  (user-export! 'track-genre track-genre)
  (user-export! 'track-id track-id)
  (user-export! 'track-last-modified track-last-modified)
  (user-export! 'track-name track-name)
  (user-export! 'track-performer track-performer)
  (user-export! 'track-pos track-pos)
  (user-export! 'track-prio track-prio)
  (user-export! 'track-start track-start)
  (user-export! 'track-title track-title)
  (user-export! 'track-track track-track)
  (user-export! 'unbind! user-unbind!)
  (user-export! 'update! (return-void update!))
  (user-export! 'win-move! (return-void win-move!))
  (user-export! 'win-bottom! (return-void win-bottom!))
  (user-export! 'win-top! (return-void win-top!))
  (user-export! 'win-activate! (thunk (window-activate! (current-window))))
  (user-export! 'win-deactivate! (thunk (window-deactivate! (current-window))))
  (user-export! 'win-add! (return-void win-add!))
  (user-export! 'win-remove! (return-void win-remove!))
  (user-export! 'win-clear! (return-void win-clear!))
  (user-export! 'win-move-tracks! (return-void win-move-tracks!))
  (user-export! 'win-search! (return-void win-search!))
  (user-export! 'win-search-next! (return-void win-search-next!))
  (user-export! 'win-search-prev! (return-void win-search-prev!))
  (user-export! 'win-edit! (return-void win-edit!))
  (user-export! 'win-sel-pos (lambda () (window-sel-pos (current-window))))
  (user-export! 'win-selected (lambda () (window-selected (current-window))))
  (user-export! 'win-mark! (thunk (window-mark! (current-window))))
  (user-export! 'win-unmark! (thunk (window-unmark! (current-window))))
  (user-export! 'win-toggle-mark! (thunk (window-toggle-mark! (current-window))))
  (user-export! 'win-clear-marked! (return-void win-clear-marked!))
  (user-export! '*win-marked (lambda () (*window-marked (current-window))))
  (user-export! 'win-marked (lambda () (window-marked (current-window))))
  (user-export! 'write-config! (return-void write-config!))
  (user-export! 'xfade scmus-xfade))

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
