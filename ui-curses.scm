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

(declare (export current-window curses-update cursor-off cursor-on
                 exit-curses get-window init-curses redraw-ui set-view!
                 connect!))

(import drewt.ncurses)
(import scmus.base scmus.client scmus.command-line scmus.editable scmus.error
        scmus.event scmus.format scmus.input scmus.keys scmus.option
        scmus.status scmus.tui)

(: get-color-option (symbol -> (list-of fixnum)))
(define (get-color-option name)
  (let ((option (get-option name)))
    (assert (list? option))
    (list (attr->number (car option))
          (safe-color->number (cadr option))
          (safe-color->number (caddr option)))))

(: update-colors! thunk)
(define (update-colors!)
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
  (void))

;; screen updates {{{

(define current-line (make-format-text (get-format 'format-current)
                                       (current-track)
                                       'cursed CURSED-TITLELINE))
(define status-line (make-format-text (get-format 'format-status)
                                      (current-track)
                                      'cursed CURSED-STATUSLINE))
(define root-widget (make-frame 'body view-widget
                                'footer (make-pile (list current-line status-line))))

(: update-current-line thunk)
(define (update-current-line)
  (set! (format-text-format current-line) (get-format 'format-current))
  (set! (format-text-data current-line) (current-track)))

(: update-status-line thunk)
(define (update-status-line)
  (set! (format-text-format status-line) (get-format 'format-status))
  (set! (format-text-data status-line) (current-track)))

(: update-status thunk)
(define (update-status)
  (set! (window-data (get-window 'status)) (alist->kv-rows (current-status)))
  (update-status-line))

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
  (when (> (LINES) 1)
    (for-each reprint-widget! (damaged-widgets))
    (clear-damaged-widgets!))
  (update-cursor!))

(: redraw-ui thunk)
(define (redraw-ui)
  (print-widget! root-widget 0 0 (COLS) (- (LINES) 1))
  (update-cursor!)
  (update-current-line)
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

(: handle-key (fixnum -> undefined))
(define (handle-key key)
  (cond
    ((= key KEY_RESIZE) (redraw-ui))
    (else
      (case (input-mode)
        ((normal-mode) (handle-input (widget-focus view-widget) key))
        ((edit-mode)   (editable-key (current-editable) key))))))

(: handle-char (char -> undefined))
(define (handle-char ch)
  (case (input-mode)
    ((normal-mode) (handle-input (widget-focus view-widget) ch))
    ((edit-mode)   (editable-char (current-editable) ch))))

(: *handle-input (-> undefined))
(define (*handle-input)
  (let-values (((ch rc) (get-char)))
    (cond
      ((= rc KEY_CODE_YES) (handle-key ch))
      ((not (= rc ERR))    (handle-char (integer->char ch))))))

(: curses-update thunk)
(define (curses-update)
  (let ((err (handle-events!)))
    (if err (scmus-error-set! err)))
  (update-tui!)
  (*handle-input))

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
  (set-view! 'queue)
  (redraw-ui)
  (print-widget! root-widget 0 0 (COLS) (- (LINES) 3))
  (set-input-mode! 'normal-mode))

(: exit-curses thunk)
(define (exit-curses)
  (handle-exceptions exn (void)
    (endwin)))

(define-view status
  (make-frame 'body   (make-window 'data (alist->kv-rows (current-status))
                                   'format *key-value-format*
                                   'cursed CURSED-WIN)
              'header (make-text " MPD Status" 'cursed CURSED-WIN-TITLE)))

(define (list->rows lines)
  (map (lambda (line) `(row . ((text . ,line)))) lines))

(define-view error
  (make-frame 'body   (make-window 'data   (list->rows (string-split-lines (scmus-error)))
                                   'cursed CURSED-WIN)
              'header (make-text " Error" 'cursed CURSED-WIN-TITLE)))

(define-event-handler command-line-changed () update-command-line)
(define-event-handler current-line-changed () update-current-line)
(define-event-handler color-changed () update-colors!)
(define-event-handler format-changed () redraw-ui)
(define-event-handler db-changed () update-db)
(define-event-handler status-changed () update-status)
(define-event-handler (error-changed) ()
  (set! (window-data (get-window 'error))
    (list->rows (string-split-lines (scmus-error)))))
