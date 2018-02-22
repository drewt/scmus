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

(: update-colors! thunk)
(define (update-colors!)
  (define (*update-colors!)
    (let loop ((i 1))
      (when (<= i NR-CURSED)
        (init_pair i (cursed-fg i) (cursed-bg i))
        (loop (+ i 1)))))
  (init-cursed! CURSED-CMDLINE     (get-color-option 'color-cmdline))
  (init-cursed! CURSED-ERROR       (get-color-option 'color-error))
  (init-cursed! CURSED-INFO        (get-color-option 'color-info))
  (init-cursed! CURSED-STATUSLINE  (get-color-option 'color-statusline))
  (init-cursed! CURSED-TITLELINE   (get-color-option 'color-titleline))
  (init-cursed! CURSED-WIN         (get-color-option 'color-win))
  (init-cursed! CURSED-WIN-CUR     (get-color-option 'color-win-cur))
  (init-cursed! CURSED-WIN-CUR-SEL (get-color-option 'color-win-cur-sel))
  (init-cursed! CURSED-WIN-SEL     (get-color-option 'color-win-sel))
  (init-cursed! CURSED-WIN-MARKED  (get-color-option 'color-win-marked))
  (init-cursed! CURSED-WIN-TITLE   (get-color-option 'color-win-title))
  (*update-colors!)
  (void))

;; windows {{{

(define root-widget #f)

(: get-window (symbol -> window))
(define (get-window view-name)
  (widget-focus (alist-ref view-name *views*)))

(: current-view (-> frame))
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
    (widget-wrap-swap! root-widget (get-view view-name))
    (widget-damaged! (get-view view-name))))

;; windows }}}
;; screen updates {{{

; TODO: Replace with text widget.
(: update-current-line thunk)
(define (update-current-line)
  (when (> (LINES) 2)
    (with-cursed CURSED-TITLELINE
      (print-line! (scmus-format (get-format 'format-current)
                                 (COLS)
                                 (current-track))
                   0
                   (- (LINES) 3)
                   (COLS)))))

(: update-status-line thunk)
(define (update-status-line)
  (when (> (LINES) 1)
    (with-cursed CURSED-STATUSLINE
      (print-line! (scmus-format (get-format 'format-status)
                                 (COLS)
                                 (current-track))
                   0
                   (- (LINES) 2)
                   (COLS)))))

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
   (with-cursed (case (command-line-mode)
                  ((info)  CURSED-INFO)
                  ((error) CURSED-ERROR)
                  (else    CURSED-CMDLINE))
     (move (- (LINES) 1) 0)
     (clrtoeol)
     (addch (case (command-line-mode)
              ((eval)    #\$)
              ((search)  #\/)
              ((command) #\:)
              (else #\space)))
     (format-addstr! (string-truncate (command-line-text)
                                      (- (COLS) 2))))))

(: update-db thunk)
(define (update-db)
  (scmus-update-stats!))

(define (update-cursor!)
  (when (current-editable)
    (let ((pos (cursor-pos)))
      (move (car pos) (cdr pos)))))

; TODO: move this logic into TUI module
(define (update-tui!)
  (when (> (LINES) 3)
    (for-each reprint-widget! (damaged-widgets))
    (clear-damaged-widgets!))
  (update-cursor!))

(: redraw-ui thunk)
(define (redraw-ui)
  (print-widget! root-widget 0 0 (COLS) (- (LINES) 3))
  (update-cursor!)
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
  (update-tui!)
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
  (set! root-widget (make-widget-wrap (get-view 'queue)))
  (redraw-ui)
  (print-widget! root-widget 0 0 (COLS) (- (LINES) 3))
  (set-input-mode! 'normal-mode))

(: exit-curses thunk)
(define (exit-curses)
  (handle-exceptions exn (void)
    (endwin)))

(define-view status
  (make-frame (make-window 'data (alist->kv-rows (current-status))
                          'format *key-value-format*
                          'cursed CURSED-WIN)
              (make-text " MPD Status" 'cursed CURSED-WIN-TITLE)))

(define (list->rows lines)
  (map (lambda (line) `(row . ((text . ,line)))) lines))

(define-view error
  (make-frame (make-window 'data   (list->rows (string-split-lines (scmus-error)))
                           'cursed CURSED-WIN)
              (make-text " Error" 'cursed CURSED-WIN-TITLE)))

(define-event-handler command-line-changed () update-command-line)
(define-event-handler current-line-changed () update-current)
(define-event-handler color-changed () update-colors!)
(define-event-handler format-changed () redraw-ui)
(define-event-handler db-changed () update-db)
(define-event-handler status-changed () update-status)
(define-event-handler (error-changed) ()
  (set! (window-data (get-window 'error))
    (list->rows (string-split-lines (scmus-error)))))
