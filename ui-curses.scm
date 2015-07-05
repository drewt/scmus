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
                 get-window init-curses push! set-view! set-window!
                 ui-initialized? update-view! win-add! win-bottom! win-clear!
                 win-clear-marked! win-edit! win-move! win-move-tracks!
                 win-remove! win-search! win-search-next! win-search-prev!
                 win-top!))

(define *ui-initialized* #f)
(define *current-view* 'queue)

;; user functions {{{

(define (push! str)
  (enter-eval-mode)
  (command-line-text-set! str))

(define (win-move! nr #!optional (relative #f))
  (assert (integer? nr) "win-move!" nr)
  (let ((nr-lines (if relative
                    (integer-scale (window-nr-lines (current-window)) nr)
                    nr)))
    (if (> nr-lines 0)
      (window-move-down! (current-window) nr-lines)
      (window-move-up! (current-window) (abs nr-lines)))))

(define (win-bottom!)
  (let ((window (current-window)))
    (window-select! window (- (window-data-len window) 1))))

(define (win-top!)
  (window-select! (current-window) 0))

(define (win-clear-marked!)
  (window-clear-marked! (current-window))
  (redraw-ui))

(define (win-search! query)
  (window-search-init! (current-window) query)
  (win-search-next!))

(define (win-search-next!)
  (let ((i (window-next-match! (current-window))))
    (when i
      (window-select! (current-window) i))))

(define (win-search-prev!)
  (let ((i (window-prev-match! (current-window))))
    (when i
      (window-select! (current-window) i))))

(define (win-add!)
  (let ((view (current-view)))
    ((view-add view) (view-window view))))

(define (win-remove!)
  (let ((view (current-view)))
    ((view-remove view) (view-window view))))

(define (win-clear!)
  (let ((view (current-view)))
    ((view-clear view) (view-window view))))

(define (win-move-tracks!)
  (let ((view (current-view)))
    ((view-move view) (view-window view))))

(define (win-edit!)
  (let ((view (current-view)))
    ((view-edit view) (view-window view))))

;; user functions }}}
;; windows {{{

(define (get-window view-name)
  (view-window (alist-ref view-name *views*)))

(define (set-window! view-name window)
  (window-nr-lines-set! window (max 0 (- (LINES) 4)))
  (view-window-set! (alist-ref view-name *views*) window))

(define (view-print-title! view)
  (cursed-set! CURSED-WIN-TITLE)
  (track-print-line 0 (view-title-fmt view) '() CURSED-WIN-TITLE))

(define (update-view! view-name)
  (if (current-view? view-name)
    (print-view! (alist-ref view-name *views*))))

(define (current-view)
  (alist-ref *current-view* *views*))

(define (current-window)
  (get-window *current-view*))

(define (current-view? view-name)
  (eqv? view-name *current-view*))

(define (set-view! view-name)
  (when (memv view-name *view-names*)
    (set! *current-view* view-name)
    (window-changed! (get-window view-name))))

;; windows }}}
;; screen updates {{{

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

(define (update-current-line)
  (when (> (LINES) 2)
    (cursed-set! CURSED-TITLELINE)
    (track-print-line (- (LINES) 3)
                      (get-format 'format-current)
                      *current-track*
                      CURSED-TITLELINE)))

(define (update-status-line)
  (when (> (LINES) 1)
    (cursed-set! CURSED-STATUSLINE)
    (track-print-line (- (LINES) 2)
                      (get-format 'format-status)
                      *current-track*
                      CURSED-STATUSLINE)))

(define (update-status)
  (update-status-line)
  (update-view! 'status))

(define (update-current)
  (update-current-line))

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

(define (update-db)
  (scmus-update-stats!)
  (update-library!)
  (update-browser!)
  (update-view! 'library))

(define (update-cursor)
  (if (current-editable)
    (let ((pos (cursor-pos)))
      (move (car pos) (cdr pos)))))

(define (redraw-ui)
  (for-each (lambda (x) (window-nr-lines-set! (view-window (cdr x))
                                              (max 0 (- (LINES) 4))))
            *views*)
  (print-view! (alist-ref *current-view* *views*))
  (update-current)
  (update-status)
  (update-command-line))

;; screen updates }}}

(define (ui-initialized?) *ui-initialized*)

(define (cursor-on)
  (curs_set 1))

(define (cursor-off)
  (curs_set 0))

(define (curses-update)
  (handle-events!)
  (update-cursor)
  (handle-input *current-view*))

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

(define (exit-curses)
  (handle-exceptions exn (void)
    (endwin)))

(define-view status
  (make-view (make-window get-data: (lambda (w) *mpd-status*)
                          changed:  (lambda (w) (register-event! 'status-changed)))
             "MPD Status"
             alist-print-line))

(define-view error
  (make-view (make-window get-data: (lambda (w) (string-split-lines *scmus-error*))
                          changed:  (lambda (w) (register-event! 'error-changed)))
             "Error"
             list-window-print-row))

(define-event command-line-changed update-command-line)
(define-event current-line-changed update-current)
(define-event color-changed update-colors!)
(define-event format-changed redraw-ui)
(define-event db-changed update-db)
(define-event status-changed update-status)
(define-event (error-changed)
  (window-data-len-update! (get-window 'error))
  (update-view! 'error))
