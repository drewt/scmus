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

(require-extension coops)

(declare (unit ui-curses)
         (uses bindings-view browser-view command-line eval-mode event format
               input keys library-view ncurses option options-view queue-view
               scmus-client scmus-error search-view ui-lib view window)
         (export current-view current-window curses-update cursor-off cursor-on
                 exit-curses get-window init-curses redraw-ui set-view!
                 connect!))

(import scmus-base command-line editable event input ncurses scmus-error)

(define *current-view* 'queue)

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

(define-method (print-widget! (view <view>) x y cols rows)
  (print-line! (scmus-format (view-title-fmt view) cols (view-title-data view))
               x
               y
               cols
               CURSED-WIN-TITLE)
  (when (> rows 1)
    (print-widget! (view-widget view) x (+ 1 y) cols (- rows 1))))

(define-method (print-widget! (separator <separator>) x y cols rows)
  (let loop ((row y))
    (when (< (- row y) rows)
      (move row x)
      (let loop ((col 0))
        (when (< col cols)
          (addch (separator-char separator))
          (loop (+ col 1))))
      (loop (+ row 1)))))

(define-method (print-widget! (container <container>) x y cols rows)
  (define (adjust-positions layout)
    (append (list (car layout)
                  (+ x (cadr layout))
                  (+ y (caddr layout)))
            (cdddr layout)))
  (let loop ((children (compute-layout container cols rows)))
    (unless (null? children)
      (apply print-widget! (adjust-positions (car children)))
      (loop (cdr children)))))

(define-method (print-widget! (window <window>) x y cols rows)
  (let loop ((rows (window-top window))
             (lines (window-nr-lines window)))
    (when (> lines 0)
      (let ((line-nr (+ y (- (window-nr-lines window) lines))))
        (if (null? rows)
          (begin
            (cursed-set! CURSED-WIN)
            (print-line! "" x line-nr cols CURSED-WIN))
          (let ((cursed (window-cursed window (car rows) line-nr)))
            (print-line! (window-print-line window (car rows) cols)
                         x
                         line-nr
                         cols
                         cursed)))
        (loop (if (null? rows) '() (cdr rows)) (- lines 1))))))

(: print-line! (string fixnum fixnum fixnum fixnum -> undefined))
(define (print-line! str col line nr-cols cursed)
  (move line col)
  (cursed-set! cursed)
  (let ((written (format-addstr! (string-truncate str nr-cols) cursed)))
    (when (< written nr-cols)
      (addstr (make-string (- nr-cols written) #\space)))))

(: update-current-line thunk)
(define (update-current-line)
  (when (> (LINES) 2)
    (cursed-set! CURSED-TITLELINE)
    (print-line! (scmus-format (get-format 'format-current)
                               (COLS)
                               *current-track*)
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
                               *current-track*)
                 0
                 (- (LINES) 2)
                 (COLS)
                 CURSED-STATUSLINE)))

(: update-status thunk)
(define (update-status)
  (set! (*window-data (get-window 'status)) (alist->kv-rows *mpd-status*))
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
             ((eval)   #\:)
             ((search) #\/)
             (else #\space)))
    (format-addstr! (string-truncate (command-line-text)
                                     (- (COLS) 2))
                    cursed))))

(: update-db thunk)
(define (update-db)
  (scmus-update-stats!)
  (update-library!)
  (update-browser!))

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
    ((normal-mode) (normal-mode-char view ch))
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
  (make-view (make-window 'data (alist->kv-rows *mpd-status*)
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
  (set! (*window-data (get-window 'error))
    (list->rows (string-split-lines (scmus-error)))))
