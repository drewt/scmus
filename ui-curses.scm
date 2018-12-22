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

(declare (export connect!
                 current-search-query
                 curses-update
                 exit-curses
                 init-curses))

(import drewt.ncurses
        scmus.base
        scmus.client
        scmus.command-line
        scmus.error
        scmus.event
        scmus.format
        scmus.keys
        scmus.option
        scmus.status
        scmus.tui
        scmus.view
        scmus.widgets)

(define current-line (make-format-text (get-format 'format-current)
                                       (current-track)
                                       'cursed CURSED-TITLELINE))
(define status-line (make-format-text (get-format 'format-status)
                                      (current-track)
                                      'cursed CURSED-STATUSLINE))
(define foot-pile (make-pile (list current-line status-line command-line-widget)))
(define root-widget (make-frame 'body view-widget
                                'footer foot-pile))

(define current-search-query (make-parameter #f))

;; If an operation is likely to stall the UI, then this macro can be used to
;; inform the user about what is going on during that time.
(define-syntax with-info-message
  (syntax-rules ()
    ((with-info-message message first rest ...)
       (call-with-info-message message (lambda () first rest ...)))))

(define (call-with-info-message message thunk)
  (command-line-print-info! message)
  (reprint-widget! command-line-widget)
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

(: curses-update thunk)
(define (curses-update)
  (let ((err (handle-events!)))
    (if err (scmus-error err)))
  (update-ui root-widget))

(define (get-color-option name)
  (let ((option (get-option name)))
    (assert (list? option))
    (list (attr->number (car option))
          (safe-color->number (cadr option))
          (safe-color->number (caddr option)))))

 (define (update-colors!)
  (palette-set!
    `((,CURSED-CMDLINE     . ,(get-color-option 'color-cmdline))
      (,CURSED-ERROR       . ,(get-color-option 'color-error))
      (,CURSED-INFO        . ,(get-color-option 'color-info))
      (,CURSED-STATUSLINE  . ,(get-color-option 'color-statusline))
      (,CURSED-TITLELINE   . ,(get-color-option 'color-titleline))
      (,CURSED-WIN         . ,(get-color-option 'color-win))
      (,CURSED-WIN-CUR     . ,(get-color-option 'color-win-cur))
      (,CURSED-WIN-CUR-SEL . ,(get-color-option 'color-win-cur-sel))
      (,CURSED-WIN-SEL     . ,(get-color-option 'color-win-sel))
      (,CURSED-WIN-MARKED  . ,(get-color-option 'color-win-marked))
      (,CURSED-WIN-TITLE   . ,(get-color-option 'color-win-title)))))

(: init-curses thunk)
(define (init-curses)
  (init-ui root-widget)
  (update-colors!)
  (ui-initialized? #t)
  (init-views!)
  (set-view! 'queue)
  (draw-ui root-widget))

(: exit-curses thunk)
(define (exit-curses)
  (handle-exceptions exn (void)
    (endwin)))

(define *key-value-format* (process-format "~-50%{key} ~{value}"))

(define (make-status-rows)
  (map (lambda (pair)
         (make-window-row `((key   . ,(car pair))
                            (value . ,(cdr pair)))
                          'key-value
                          (lambda (_) *key-value-format*)))
       (current-status)))

(define *status-window*
  (make <window> 'data (make-status-rows)
                 'cursed CURSED-WIN
                 'cursed-fn (win-cursed-fn)))

(define-view status
  (make-frame 'body   *status-window*
              'header (make-text " MPD Status" 'cursed CURSED-WIN-TITLE)))

(define (make-error-rows)
  (map (lambda (line)
         (make-text line 'cursed CURSED-WIN))
       (string-split (scmus-error) "\n")))

(define *error-text* (make-text "" 'cursed CURSED-WIN))

(define-view error
  (make-frame 'body   *error-text*
              'header (make-text " Error" 'cursed CURSED-WIN-TITLE)))

(define-event-handler (current-track-changed) ()
  (set! (format-text-format current-line) (get-format 'format-current))
  (set! (format-text-data current-line) (current-track)))

(define-event-handler (status-changed) ()
  (set! (window-data *status-window*) (make-status-rows))
  (set! (format-text-format status-line) (get-format 'format-status))
  (set! (format-text-data status-line) (current-track)))

(define-event-handler (error-changed) ()
  (set! (text-text *error-text*) (scmus-error)))

(define-event-handler color-changed () update-colors!)

(define-event-handler db-changed () scmus-update-stats!)

(define-event-handler (format-changed) ()
  (draw-ui root-widget))
