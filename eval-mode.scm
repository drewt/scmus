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

(require-extension ncurses sandbox srfi-13)
 
(declare (unit eval-mode)
         (uses ui-curses scmus-client command-line keys option window)
         (export init-sandbox user-load enter-eval-mode leave-eval-mode
                 eval-mode-char eval-mode-key))

;; user functions {{{

(define (user-bind! keys context thunk #!optional (force #f))
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (begin
        (if force
          (unbind! key-list context))
        (make-binding! key-list context thunk))
      #f)))

(define (user-unbind! keys context)
  (let ((key-list (string-tokenize keys)))
    (if (binding-keys-valid? key-list)
      (unbind! key-list context)
      #f)))

(define (push! str)
  (set-input-mode! 'eval-mode)
  (command-line-text-set! str))

(define (echo! arg)
  (define (clean-text text)
    (string-delete (lambda (x)
                     (case x
                       ((#\newline #\linefeed) #t)
                       (else #f)))
                   text))
  (command-line-text-set! (clean-text (format #f "~a" arg))))

(define (colorscheme! str)
  (user-load (format "~a/colors/~a.scm" *scmus-dir* str)))

(define (shell! command  . args)
  (process-fork
    (lambda ()
      (handle-exceptions exn (void)
        (process-execute command args)))))

;; Synchronous version of shell!
(define (shell-sync! command . args)
  (nth-value 2 (process-wait (apply shell! command args))))

(define (update! #!optional (path #f))
  (scmus-update! path))

(define (rescan! #!optional (path #f))
  (scmus-rescan! path))

;; user functions }}}

(define *user-env* (make-safe-environment parent: default-safe-environment))

(define (user-export! name obj)
  (safe-environment-set! *user-env* name obj))

(define (init-sandbox)
  (user-export! 'bind! user-bind!)
  (user-export! 'clear-queue! scmus-clear!)
  (user-export! 'colorscheme! colorscheme!)
  (user-export! 'consume-set! scmus-consume-set!)
  (user-export! 'current-track current-track)
  (user-export! 'echo! echo!)
  (user-export! 'get-option get-option)
  (user-export! 'next! scmus-next!)
  (user-export! 'pause! scmus-pause!)
  (user-export! 'play! scmus-play!)
  (user-export! 'prev! scmus-prev!)
  (user-export! 'push! push!)
  (user-export! 'queue-delete! scmus-delete!)
  (user-export! 'queue-delete-id! scmus-delete-id!)
  (user-export! 'queue-delete-range! scmus-delete-range!)
  (user-export! 'queue-move! scmus-move!)
  (user-export! 'queue-move-id! scmus-move-id!)
  (user-export! 'queue-move-range! scmus-move-range!)
  (user-export! 'queue-swap! scmus-swap!)
  (user-export! 'queue-swap-id! scmus-swap-id!)
  (user-export! 'random-set! scmus-random-set!)
  (user-export! 'repeat-set! scmus-repeat-set!)
  (user-export! 'rescan! rescan!)
  (user-export! 'seek! scmus-seek!)
  (user-export! 'set-option! set-option!)
  (user-export! 'set-view! set-view!)
  (user-export! 'shell! shell!)
  (user-export! 'shell-sync! shell-sync!)
  (user-export! 'shuffle! scmus-shuffle!)
  (user-export! 'single-set! scmus-single-set!)
  (user-export! 'stop! scmus-stop!)
  (user-export! 'toggle-consume! scmus-toggle-consume!)
  (user-export! 'toggle-random! scmus-toggle-random!)
  (user-export! 'toggle-repeat! scmus-toggle-repeat!)
  (user-export! 'toggle-single! scmus-toggle-single!)
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
  (user-export! 'update! update!)
  (user-export! 'win-move! win-move!)
  (user-export! 'win-bottom! win-bottom!)
  (user-export! 'win-top! win-top!)
  (user-export! 'win-activate! (lambda () (window-activate! (current-window))))
  (user-export! 'win-deactivate! (lambda () (window-deactivate! (current-window))))
  (user-export! 'win-add! win-add!)
  (user-export! 'win-remove! win-remove!)
  (user-export! 'win-clear! win-clear!)
  (user-export! 'win-move-tracks! win-move-tracks!)
  (user-export! 'win-search! win-search!)
  (user-export! 'win-search-next! win-search-next!)
  (user-export! 'win-search-prev! win-search-prev!)
  (user-export! 'win-sel-pos (lambda () (window-sel-pos (current-window))))
  (user-export! 'win-selected (lambda () (window-selected (current-window))))
  (user-export! 'win-mark! (lambda () (window-mark! (current-window))))
  (user-export! 'win-unmark! (lambda () (window-unmark! (current-window))))
  (user-export! 'win-toggle-mark! (lambda () (window-toggle-mark! (current-window))))
  (user-export! 'win-clear-marked! win-clear-marked!)
  (user-export! '*win-marked (lambda () (*window-marked (current-window))))
  (user-export! 'win-marked (lambda () (window-marked (current-window)))))

(define (user-eval str)
  (assert (string? str))
  (condition-case (safe-eval (read (open-input-string str))
                             environment: *user-env*)
    (e () (error-set! e))))

(define (user-load path)
  (assert (string? path))
  (call-with-input-file path
    (lambda (in)
      (let loop ()
       (let ((input (read in)))
         (unless (eqv? input #!eof)
           (condition-case (safe-eval input environment: *user-env*)
             (e () (void)))
           (loop)))))))

(define (enter-eval-mode)
  (command-line-clear!)
  (cursor-on))

(define (leave-eval-mode)
  (command-line-clear!)
  (set-input-mode! 'normal-mode))

(define (eval-mode-char ch)
  (assert (char? ch))
  (case ch
    ((#\newline)
      (let ((cmdline (command-line-text)))
        (leave-eval-mode)
        (user-eval cmdline)))
    ((#\esc)
      (leave-eval-mode))
    ((#\backspace)
      (if (command-line-empty?)
        (leave-eval-mode)
        (command-line-char ch)))
    (else
      (command-line-char ch))))

(define (eval-mode-key key)
  (cond
    ((key= key KEY_UP) (void))
    ((key= key KEY_DOWN) (void))
    ((key= key KEY_BACKSPACE)
      (if (command-line-empty?)
        (leave-eval-mode)
        (command-line-key key)))
    (else (command-line-key key))))
