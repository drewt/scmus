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

(declare (unit ui-curses)
         (uses bindings-view browser-view command-line eval-mode event format
               input keys library-view ncurses option options-view queue-view
               scmus-client search-view ui-lib view window)
         (export current-window curses-update cursor-off cursor-on exit-curses
                 get-window init-curses push! redraw-ui set-view! set-window!
                 ui-initialized? update-view! win-add! win-bottom! win-clear!
                 win-clear-marked! win-edit! win-move! win-move-tracks!
                 win-remove! win-search! win-search-next! win-search-prev!
                 win-top!))

(define *ui-initialized* #f)
(define *current-view* 'queue)

;; user functions {{{

(: push! (string -> undefined))
(define (push! str)
  (enter-eval-mode)
  (command-line-text-set! str))

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

(: win-top! thunk)
(define (win-top!)
  (window-select! (current-window) 0))

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

(: win-add! thunk)
(define (win-add!)
  (let ((view (current-view)))
    ((view-add view) (view-window view))))

(: win-remove! thunk)
(define (win-remove!)
  (let ((view (current-view)))
    ((view-remove view) (view-window view))))

(: win-clear! thunk)
(define (win-clear!)
  (let ((view (current-view)))
    ((view-clear view) (view-window view))))

(: win-move-tracks! (#!optional boolean -> undefined))
(define (win-move-tracks! #!optional (before #f))
  (let ((view (current-view)))
    ((view-move view) (view-window view) before)))

(: win-edit! thunk)
(define (win-edit!)
  (let ((view (current-view)))
    ((view-edit view) (view-window view))))

;; user functions }}}
;; windows {{{

(: get-window (symbol -> window))
(define (get-window view-name)
  (view-window (alist-ref view-name *views*)))

(: set-window! (symbol window -> undefined))
(define (set-window! view-name window)
  (window-nr-lines-set! window (max 0 (- (LINES) 4)))
  (view-window-set! (alist-ref view-name *views*) window))

(: view-print-title! (view -> undefined))
(define (view-print-title! view)
  (cursed-set! CURSED-WIN-TITLE)
  (track-print-line 0 (view-title-fmt view) '() CURSED-WIN-TITLE))

(: update-view! (symbol -> undefined))
(define (update-view! view-name)
  (if (current-view? view-name)
    (print-view! (alist-ref view-name *views*))))

(: current-view (-> view))
(define (current-view)
  (alist-ref *current-view* *views*))

(: current-window (-> window))
(define (current-window)
  (get-window *current-view*))

(: current-view? (symbol -> boolean))
(define (current-view? view-name)
  (eqv? view-name *current-view*))

(: set-view! (symbol -> undefined))
(define (set-view! view-name)
  (when (memv view-name *view-names*)
    (set! *current-view* view-name)
    (window-changed! (get-window view-name))))

;; windows }}}
;; screen updates {{{

(: print-view! (view -> undefined))
(define (print-view! view)
  (when (> (LINES) 3)
    (view-print-title! view)
    (let* ((window (view-window view))
           (nr-lines (window-nr-lines window)))
      (let loop ((rows (window-top window)) (lines nr-lines))
        (when (> lines 0)
          (let ((line-nr (- nr-lines (- lines 1))))
            (if (null? rows)
              (begin (cursed-set! CURSED-WIN)
                     (move line-nr 0)
                     (clrtoeol))
              (let ((cursed (view-cursed-set! view (car rows) line-nr)))
                (view-print-line! view (car rows) line-nr cursed)))
            (loop (if (null? rows) '() (cdr rows)) (- lines 1))))))))

(: update-current-line thunk)
(define (update-current-line)
  (when (> (LINES) 2)
    (cursed-set! CURSED-TITLELINE)
    (track-print-line (- (LINES) 3)
                      (get-format 'format-current)
                      *current-track*
                      CURSED-TITLELINE)))

(: update-status-line thunk)
(define (update-status-line)
  (when (> (LINES) 1)
    (cursed-set! CURSED-STATUSLINE)
    (track-print-line (- (LINES) 2)
                      (get-format 'format-status)
                      *current-track*
                      CURSED-STATUSLINE)))

(: update-status thunk)
(define (update-status)
  (update-status-line)
  (update-view! 'status))

(: update-current thunk)
(define (update-current)
  (update-current-line))

(: update-command-line thunk)
(define (update-command-line)
  (cursed-set! CURSED-CMDLINE)
  (cursed-set! (case (command-line-mode)
                 ((info) CURSED-INFO)
                 ((error) CURSED-ERROR)
                 (else CURSED-CMDLINE)))
  (move (- (LINES) 1) 0)
  (clrtoeol)
  (addch (case (command-line-mode)
           ((eval)   #\:)
           ((search) #\/)
           (else #\space)))
  (addstr (string-truncate (command-line-text)
                           (- (COLS) 2))))

(: update-db thunk)
(define (update-db)
  (scmus-update-stats!)
  (update-library!)
  (update-browser!)
  (update-view! 'library))

(: update-cursor thunk)
(define (update-cursor)
  (if (current-editable)
    (let ((pos (cursor-pos)))
      (move (car pos) (cdr pos)))))

(: redraw-ui thunk)
(define (redraw-ui)
  (for-each (lambda (x) (window-nr-lines-set! (view-window (cdr x))
                                              (max 0 (- (LINES) 4))))
            *views*)
  (print-view! (alist-ref *current-view* *views*))
  (update-current)
  (update-status)
  (update-command-line))

;; screen updates }}}

(: ui-initialized? (-> boolean))
(define (ui-initialized?) *ui-initialized*)

(: cursor-on (#!optional (pair fixnum fixnum) -> undefined))
(define (cursor-on #!optional (pos #f))
  (when pos
    (move (car pos) (cdr pos)))
  (curs_set 1))

(: cursor-off thunk)
(define (cursor-off)
  (curs_set 0))

(: curses-update thunk)
(define (curses-update)
  (handle-events!)
  (update-cursor)
  (handle-input *current-view*))

(: init-curses thunk)
(define (init-curses)
  (initscr)
  (set! *ui-initialized* #t)
  (cbreak)
  (keypad (stdscr) #t)
  (halfdelay 5)
  (noecho)
  (when (has_colors)
    (start_color)
    (use_default_colors))
  (update-colors!)
  (init-views!)
  (redraw-ui)
  (set-input-mode! 'normal-mode))

(: exit-curses thunk)
(define (exit-curses)
  (handle-exceptions exn (void)
    (endwin)))

(define-view status
  (make-view (make-window data-thunk: (lambda (w) *mpd-status*)
                          changed:    (lambda (w) (register-event! 'status-changed)))
             "MPD Status"
             print-line: alist-print-line))

(define-view error
  (make-view (make-window data-thunk: (lambda (w) (string-split-lines *scmus-error*))
                          changed:    (lambda (w) (register-event! 'error-changed)))
             "Error"))

(define-event command-line-changed update-command-line)
(define-event current-line-changed update-current)
(define-event color-changed update-colors!)
(define-event format-changed redraw-ui)
(define-event db-changed update-db)
(define-event status-changed update-status)
(define-event (error-changed)
  (window-data-len-update! (get-window 'error))
  (update-view! 'error))
