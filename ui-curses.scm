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

(declare (export current-view current-window curses-update cursor-off cursor-on
                 exit-curses get-window init-curses redraw-ui set-view!
                 connect!))

(import drewt.ncurses)
(import scmus.base scmus.client scmus.command-line scmus.editable scmus.error
        scmus.event scmus.format scmus.input scmus.keys scmus.option
        scmus.status scmus.tui)

(define *current-view* 'queue)

(: get-color-option (symbol -> (list-of fixnum)))
(define (get-color-option name)
  (let ((option (get-option name)))
    (assert (list? option))
    (list (attr->number (car option))
          (safe-color->number (cadr option))
          (safe-color->number (caddr option)))))

(: init-cursed! (fixnum symbol -> undefined))
(define (init-cursed! cursed color)
  (vector-set! *colors* (cursed-i cursed) (get-color-option color)))

(: update-colors! thunk)
(define (update-colors!)
  (define (*update-colors!)
    (let loop ((i 1))
      (when (<= i NR-CURSED)
        (init_pair i (cursed-fg i) (cursed-bg i))
        (loop (+ i 1)))))
  (init-cursed! CURSED-CMDLINE     'color-cmdline)
  (init-cursed! CURSED-ERROR       'color-error)
  (init-cursed! CURSED-INFO        'color-info)
  (init-cursed! CURSED-STATUSLINE  'color-statusline)
  (init-cursed! CURSED-TITLELINE   'color-titleline)
  (init-cursed! CURSED-WIN         'color-win)
  (init-cursed! CURSED-WIN-CUR     'color-win-cur)
  (init-cursed! CURSED-WIN-CUR-SEL 'color-win-cur-sel)
  (init-cursed! CURSED-WIN-SEL     'color-win-sel)
  (init-cursed! CURSED-WIN-MARKED  'color-win-marked)
  (init-cursed! CURSED-WIN-TITLE   'color-win-title)
  (*update-colors!)
  (cursed-set! CURSED-WIN)
  (void))

;; windows {{{

(: get-window (symbol -> window))
(define (get-window view-name)
  (view-window (alist-ref view-name *views*)))

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
    (widget-damaged! (get-view view-name))))

;; windows }}}
;; screen updates {{{

(: print-view! (view -> undefined))
(define (print-view! view)
  (when (> (LINES) 3)
    (print-widget! view 0 0 (COLS) (- (LINES) 3))))

; TODO: Replace with text widget.
;       But first need to implement partial redraws so we're not redrawing the whole screen
;       for current-/status-line changes
(: update-current-line thunk)
(define (update-current-line)
  (when (> (LINES) 2)
    (cursed-set! CURSED-TITLELINE)
    (print-line! (scmus-format (get-format 'format-current)
                               (COLS)
                               (current-track))
                 0
                 (- (LINES) 3)
                 (COLS)
                 CURSED-TITLELINE)))

(: update-status-line thunk)
(define (update-status-line)
  (when (> (LINES) 1)
    (cursed-set! CURSED-STATUSLINE)
    (print-line! (scmus-format (get-format 'format-status)
                               (COLS)
                               (current-track))
                 0
                 (- (LINES) 2)
                 (COLS)
                 CURSED-STATUSLINE)))

(: update-status thunk)
(define (update-status)
  (set! (window-data (get-window 'status)) (alist->kv-rows (current-status)))
  (update-status-line))

(: update-current thunk)
(define (update-current)
  (update-current-line))

(: update-command-line thunk)
(define (update-command-line)
  (when (> (COLS) 1)
   (let ((cursed (case (command-line-mode)
                  ((info)  CURSED-INFO)
                  ((error) CURSED-ERROR)
                  (else    CURSED-CMDLINE))))
    (cursed-set! cursed)
    (move (- (LINES) 1) 0)
    (clrtoeol)
    (addch (case (command-line-mode)
             ((eval)    #\$)
             ((search)  #\/)
             ((command) #\:)
             (else #\space)))
    (format-addstr! (string-truncate (command-line-text)
                                     (- (COLS) 2))
                    cursed))))

(: update-db thunk)
(define (update-db)
  (scmus-update-stats!))

(: update-cursor thunk)
(define (update-cursor)
  (if (current-editable)
    (let ((pos (cursor-pos)))
      (move (car pos) (cdr pos)))))

(define (update-current-view!)
  (let ((view (current-view)))
    (when (widget-damaged view)
      (print-view! view)
      (set! (widget-damaged view) #f))))

(: redraw-ui thunk)
(define (redraw-ui)
  (define (update-geometry view)
    (widget-geometry-set! (view-widget (cdr view))
                          (max 0 (- (COLS) 2))
                          (max 0 (- (LINES) 4))))
  (for-each update-geometry *views*)
  (print-view! (alist-ref *current-view* *views*))
  (update-current)
  (update-status)
  (update-command-line))

;; screen updates }}}

;; If an operation is likely to stall the UI, then this macro can be used to
;; inform the user about what is going on during that time.
(define-syntax with-info-message
  (syntax-rules ()
    ((with-info-message message first rest ...)
       (call-with-info-message message (lambda () first rest ...)))))

(define (call-with-info-message message thunk)
  (command-line-print-info! message)
  (update-command-line)
  (refresh)
  (let ((retval (thunk)))
    (when (string=? (command-line-text) message)
      (command-line-clear!))
    retval))

(: connect! (#!optional (or boolean string)
                        (or boolean symbol fixnum)
                        (or boolean symbol string)
              -> boolean))
(define (connect! #!optional (host #f) (port 'default) (pass 'default))
  ; host: #f means default
  ; port: #f means UNIX socket, 'default means default
  ; pass: #f means no password, 'default means default
  (let ((host (if host host (get-option 'mpd-address)))
        (port (if (eqv? port 'default) (get-option 'mpd-port) port))
        (pass (if (eqv? pass 'default) (get-option 'mpd-password) pass)))
    (with-info-message (format "Connecting to ~a:~a..." host port)
      (scmus-connect! host port pass))))

(: handle-key (symbol fixnum -> undefined))
(define (handle-key view key)
  (cond
    ((= key KEY_RESIZE) (redraw-ui))
    (else
      (case (input-mode)
        ((normal-mode) (normal-mode-key view key))
        ((edit-mode)   (editable-key (current-editable) key))))))

(: handle-char (symbol char -> undefined))
(define (handle-char view ch)
  (case (input-mode)
    ((normal-mode) (normal-mode-key view ch))
    ((edit-mode)   (editable-char (current-editable) ch))))

(: handle-input (symbol -> undefined))
(define (handle-input view)
  (let-values (((ch rc) (get-char)))
    (cond
      ((= rc KEY_CODE_YES) (handle-key view ch))
      ((= rc ERR) #f)
      (else (handle-char view (integer->char ch))))))

(: curses-update thunk)
(define (curses-update)
  (let ((err (handle-events!)))
    (if err (scmus-error-set! err)))
  (update-current-view!)
  (update-cursor)
  (handle-input *current-view*))

(: init-curses thunk)
(define (init-curses)
  (initscr)
  (ui-initialized!)
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
  (make-view (make-window 'data (alist->kv-rows (current-status))
                          'format *key-value-format*)
             " MPD Status"))

(define (list->rows lines)
  (map (lambda (line) `(row . ((text . ,line)))) lines))

(define-view error
  (make-view (make-window 'data (list->rows (string-split-lines (scmus-error))))
             " Error"))

(define-event-handler command-line-changed () update-command-line)
(define-event-handler current-line-changed () update-current)
(define-event-handler color-changed () update-colors!)
(define-event-handler format-changed () redraw-ui)
(define-event-handler db-changed () update-db)
(define-event-handler status-changed () update-status)
(define-event-handler (error-changed) ()
  (set! (window-data (get-window 'error))
    (list->rows (string-split-lines (scmus-error)))))
