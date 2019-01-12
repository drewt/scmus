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

(define current-line (make-format-text (get-option 'format-current)
                                       (current-track)
                                       'cursed CURSED-TITLELINE))
(define status-line (make-format-text (get-option 'format-status)
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
  (update-ui root-widget)
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

(let ((authenticating? #f))
  (add-listener/global 'mpd-unauthenticated
    (lambda ()
      (unless authenticating?
        (set! authenticating? #t)
        (command-line-get-string "Password: "
          (lambda (pass)
            (connect! #f 'default pass))
          finally: (lambda ()
                     (set! authenticating? #f))
          password?: #t)))))

(: curses-update thunk)
(define (curses-update)
  (let ((err (handle-events)))
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

(define *key-value-format* (compile-format-string " ~-50%{key} ~{value}"))

(define (make-status-rows)
  (map (lambda (pair)
         (make-window-row `((key   . ,(car pair))
                            (value . ,(cdr pair)))
                          'key-value
                          *key-value-format*))
       (current-status)))

(define *status-window*
  (make <window> 'data (make-status-rows)
                  'cursed CURSED-WIN
                  'cursed-fun (win-cursed-fun)))

(define-view status
  (make-frame 'body   *status-window*
              'header (make-text " MPD Status" 'cursed CURSED-WIN-TITLE)))

(add-listener/global 'track-changed
  (lambda ()
    (set! (format-text-data current-line) (current-track))))

(add-listener/global 'status-changed
  (lambda ()
    (set! (list-box-data *status-window*) (make-status-rows))
    (set! (format-text-data status-line) (current-track))))

(add-listener/global 'color-changed update-colors!)
(add-listener/global 'db-changed scmus-update-stats!)
(add-listener/global 'format-changed (lambda () (draw-ui root-widget)))

(add-option-listener 'format-current
  (lambda (option)
    (set! (format-text-format current-line)
          (option-value option))))

(add-option-listener 'format-status
  (lambda (option)
    (set! (format-text-format status-line)
          (option-value option))))
