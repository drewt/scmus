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

(require-extension ncurses)

(declare (unit ui-curses)
         (uses scmus-client
               eval-mode
               search-mode
               command-line
               keys
               format
               option
               window)
         (export *ui-initialized*
                 *current-input-mode*
                 *current-view*
                 set-view!
                 win-move!
                 win-activate!
                 win-deactivate!
                 print-command-line-char
                 register-event!
                 curses-update
                 cursor-on
                 cursor-off
                 set-input-mode!
                 handle-input
                 init-curses
                 exit-curses))

;;; definitions missing from the ncurses egg
(define bkgdset
  (foreign-lambda* void ((unsigned-long a0)) "bkgdset(a0);"))
(define use_default_colors
  (foreign-lambda* integer () "return(use_default_colors());"))

(define-constant CURSED-CMDLINE 0)
(define-constant CURSED-ERROR 1)
(define-constant CURSED-INFO 2)
(define-constant CURSED-SEPARATOR 3)
(define-constant CURSED-STATUSLINE 4)
(define-constant CURSED-TITLELINE 5)
(define-constant CURSED-WIN 6)
(define-constant CURSED-WIN-CUR 7)
(define-constant CURSED-WIN-CUR-SEL 8)
(define-constant CURSED-WIN-INACTIVE-CUR-SEL 9)
(define-constant CURSED-WIN-INACTIVE-SEL 10)
(define-constant CURSED-WIN-SEL 11)
(define-constant CURSED-WIN-TITLE 12)
(define-constant NR-CURSED 13)

(define *ui-initialized* #f)
(define *current-input-mode* 'normal-mode)
(define *current-view* 'queue)

;; alist associating views and windows
(define *windows*
  '((library . #f)
    (queue . #f)
    (status . #f)
    (error . #f)))

;; Convenient interface to make-window.  Can only be used *after* ncurses is
;; initialized.
(define-syntax make-generic-window
  (syntax-rules ()
    ((make-generic-window getter len changed-event selected-fn deactivate-fn)
      (make-window #f
                   getter
                   len
                   0
                   0
                   (- (LINES) 4)
                   (lambda (w) (register-event! changed-event))
                   selected-fn
                   deactivate-fn))))

;; make-window for global lists.  Each member of the list becomes a row in the
;; window.
(define-syntax make-list-window
  (syntax-rules ()
    ((make-list-window lst changed-event selected-fn deactivate-fn)
      (make-generic-window (lambda (w) lst)
                           (length lst)
                           changed-event
                           selected-fn
                           deactivate-fn))
    ((make-list-window lst changed-event selected-fn)
      (make-list-window lst changed-event selected-fn (lambda (w) (void))))
    ((make-list-window lst changed-event)
      (make-list-window lst changed-event
                        (lambda (w) (void))
                        (lambda (w) (void))))))

;; make-window for global strings.  Each line in the string becomes a row in
;; the window.
(define-syntax make-string-window
  (syntax-rules ()
     ((make-string-window str changed-event selected-fn deactivate-fn)
       (make-generic-window (lambda (w) (string-split-lines str))
                            (length (string-split-lines str))
                            changed-event
                            selected-fn
                            deactivate-fn))
     ((make-string-window str changed-event selected-fn)
       (make-string-window str changed-event selected-fn (lambda (w) (void))))
     ((make-string-window str changed-event)
       (make-string-window str changed-event
                           (lambda (w) (void))
                           (lambda (w) (void))))))

(define (current-window)
  (alist-ref *current-view* *windows*))

(define (current-view? view)
  (eqv? view *current-view*))

(define (set-view! view)
  (when (memv view '(library queue status error))
    (set! *current-view* view)
    (case view
      ((library) (register-event! 'library-changed))
      ((queue)   (register-event! 'queue-changed))
      ((status)  (register-event! 'status-changed))
      ((error)   (register-event! 'error-changed)))))

(define (win-move! nr-lines)
  (assert (integer? nr-lines))
  (if (> nr-lines 0)
    (window-move-down! (current-window) nr-lines)
    (window-move-up! (current-window) (abs nr-lines))))

(define (win-activate!)
  (window-activate! (current-window)))

(define (win-deactivate!)
  (window-deactivate! (current-window)))

(define-syntax let-format
  (syntax-rules ()
    ((let-format ((left right) fmt track len) body ...)
      (let* ((left-right (scmus-format fmt len track))
             (left (car left-right))
             (right (cdr left-right)))
        body ...))
    ((let-format ((left right) fmt track) body ...)
      (let-format ((left right) fmt track (- (COLS) 2))
        body ...))
    ((let-format ((left right) fmt) body ...)
      (let-format ((left right) fmt *current-track*)
        body ...))))

(define (format-print-line line fmt track)
  (assert (integer? line))
  (assert (list? fmt))
  (assert (list? track))
  (let-format ((left right) fmt track)
    (mvaddch line 0 #\space)
    (addstr left)
    (clrtoeol)
    (mvaddstr line
              (- (COLS) (string-length right) 1)
              right)))

(define (print-command-line-char ch)
  (cursed-set! CURSED-CMDLINE)
  (mvaddch (- (LINES) 1) 0 ch))

(define (selected-row? window line-nr)
  (= (window-sel-pos window)
     (+ (window-top-pos window) line-nr)))

;; Calls print-fn with window, row item, and line number for each row in
;; window.
(define (print-window window print-fn)
  (let ((nr-lines (window-nr-lines window)))
    (let loop ((rows (window-top window)) (lines nr-lines))
      (when (> lines 0)
        (let ((line-nr (- nr-lines (- lines 1))))
          (if (null? rows)
            (begin (cursed-set! CURSED-WIN)
                   (move line-nr 0)
                   (clrtoeol))
            (print-fn window (car rows) line-nr))
          (loop (if (null? rows) '() (cdr rows)) (- lines 1)))))))

;; Prints a window title from the given format.
(define (print-window-title fmt)
  (cursed-set! CURSED-WIN-TITLE)
  (format-print-line 0 fmt '()))

;; Generic function to print an alist entry, for use with print-window.
(define (alist-window-print-row window row line-nr)
  (if (selected-row? window (- line-nr 1))
    (cursed-set! CURSED-WIN-SEL)
    (cursed-set! CURSED-WIN))
  (mvaddstr line-nr 0
            (string-truncate (format " ~a" (car row))
                             (- (COLS) 2)))
  (clrtoeol)
  (mvaddstr line-nr (- (quotient (COLS) 2) 1)
            (string-truncate (format " ~a" (cdr row))
                             (- (COLS) 2))))

(define (list-window-print-row window row line-nr)
  (if (selected-row? window (- line-nr 1))
    (cursed-set! CURSED-WIN-SEL)
    (cursed-set! CURSED-WIN))
  (mvaddstr line-nr 0
            (string-truncate (format " ~a" row)
                             (- (COLS) 2)))
  (clrtoeol))

;; Set the appropriate CURSED-* pair for the given window and track.
(define (cursed-trackwin-set! window track line-nr)
  (assert (window? window))
  (assert (list? track))
  (if (null? track)
    (cursed-set! CURSED-WIN)
    (let ((current? (current-track? track))
          (selected? (selected-row? window (- line-nr 1)))
          (selected? (track= track (window-selected window))))
      (cursed-set!
        (cond
          ((and current? selected?) CURSED-WIN-CUR-SEL)
          (current?                 CURSED-WIN-CUR)
          (selected?                CURSED-WIN-SEL)
          (else                     CURSED-WIN))))))

;; Returns a function to print a track, which can be passed to print-window.
(define (make-trackwin-print-row fmt)
  (lambda (window track line-nr)
    (cursed-trackwin-set! window track line-nr)
    (format-print-line line-nr fmt track)))

(define (update-track-window window title-fmt track-fmt)
  (print-window-title title-fmt)
  (print-window window (make-trackwin-print-row track-fmt)))

(define (update-library)
  (when (current-view? 'library)
    (print-window-title (process-format (string->list "Library")))
    (print-window (alist-ref 'library *windows*)
                  list-window-print-row)))

(define (update-queue-window)
  (when (current-view? 'queue)
    (update-track-window (alist-ref 'queue *windows*)
                         (get-option 'format-queue-title)
                         (get-option 'format-queue))))

(define (update-queue-data)
  (window-data-len-update! (alist-ref 'queue *windows*))
  (update-queue-window))

(define (update-status-window)
  (when (current-view? 'status)
    (print-window-title (process-format (string->list "MPD Status")))
    (print-window (alist-ref 'status *windows*)
                  alist-window-print-row)))

(define (update-error-window)
  (when (current-view? 'error)
    (print-window-title (process-format (string->list "Error")))
    (print-window (alist-ref 'error *windows*)
                  list-window-print-row)))

(define (update-current-line)
  (cursed-set! CURSED-TITLELINE)
  (format-print-line (- (LINES) 3)
                     (get-option 'format-current)
                     *current-track*))

(define (update-status-line)
  (cursed-set! CURSED-STATUSLINE)
  (format-print-line (- (LINES) 2)
                     (get-option 'format-status)
                     *current-track*))

(define (update-status)
  (update-status-line)
  (update-status-window))

(define (update-current)
  (update-current-line))

(define (update-error)
  (window-data-len-update! (alist-ref 'error *windows*))
  (update-error-window))

(define (update-command-line)
  (cursed-set! CURSED-CMDLINE)
  (move (- (LINES) 1) 1)
  (clrtoeol)
  (addstr (string-truncate (command-line-text)
                           (- (COLS) 2))))

(define (update-cursor)
  (move (- (LINES) 1) (command-line-cursor-pos)))

(define *events* '())
(define *event-handlers*
  (list (cons 'command-line-changed update-command-line)
        (cons 'status-changed update-status)
        (cons 'current-line-changed update-current)
        (cons 'library-changed update-library)
        (cons 'queue-changed update-queue-window)
        (cons 'queue-data-changed update-queue-data)
        (cons 'error-changed update-error)))

(define (register-event! event)
  (assert (symbol? event))
  (set! *events* (cons event *events*)))

(define (curses-update)
  (for-each (lambda (x)
              ((alist-ref x *event-handlers*)))
            *events*)
  (update-cursor)
  (set! *events* '()))

(define (handle-resize)
  (for-each (lambda (x) (window-nr-lines-set! (cdr x) (- (LINES) 4)))
            *windows*)
  (register-event! 'library-changed)
  (register-event! 'queue-changed)
  (register-event! 'current-line-changed)
  (register-event! 'status-changed)
  (register-event! 'command-line-changed)
  (register-event! 'error-changed))

(define (cursor-on)
  (curs_set 1))

(define (cursor-off)
  (curs_set 0))

(define (set-input-mode! mode)
  (assert (symbol? mode))
  (assert (memv mode '(normal-mode eval-mode search-mode)))
  (case mode
    ((normal-mode) (enter-normal-mode))
    ((eval-mode) (enter-eval-mode))
    ((search-mode) (enter-search-mode)))
  (set! *current-input-mode* mode))

(define (handle-key key)
  (case *current-input-mode*
    ((normal-mode)  (normal-mode-key key))
    ((eval-mode) (eval-mode-key key))
    ((search-mode)  (search-mode-key key))))

(define (handle-char ch)
  (case *current-input-mode*
    ((normal-mode)  (normal-mode-char ch))
    ((eval-mode) (eval-mode-char ch))
    ((search-mode)  (search-mode-char ch))))

(define (handle-input)
  (let ((ch (getch)))
    (cond
      ((key= ch ERR)        #f)
      ((key= ch KEY_RESIZE) (handle-resize))
      ((key? ch)            (handle-key ch))
      (else                 (handle-char ch)))))

;; colors {{{

(define (color-symbol->number sym)
  (case sym
    ((default)       -1)
    ((black)         COLOR_BLACK)
    ((red)           COLOR_RED)
    ((green)         COLOR_GREEN)
    ((yellow)        COLOR_YELLOW)
    ((blue)          COLOR_BLUE)
    ((magenta)       COLOR_MAGENTA)
    ((cyan)          COLOR_CYAN)
    ((white)         COLOR_WHITE)
    ((dark-gray)     8)
    ((light-red)     9)
    ((light-green)   10)
    ((light-yellow)  11)
    ((light-blue)    12)
    ((light-magenta) 13)
    ((light-cyan)    14)
    ((gray)          15)
    (else            -1)))

(define *colors* (make-vector NR-CURSED))

(define (colors-set! cursed attr bg fg)
  (vector-set! *colors* cursed (list attr bg fg)))

(define (get-color-option name)
  (let ((option (get-option name)))
    (if (symbol? option)
      (color-symbol->number option)
      option)))

(define (cursed-pair cursed)
  (+ 1 cursed))

(define (cursed-attr cursed)
  (car (vector-ref *colors* cursed)))

(define (cursed-bg cursed)
  (cadr (vector-ref *colors* cursed)))

(define (cursed-fg cursed)
  (caddr (vector-ref *colors* cursed)))

(define (init-cursed! cursed attr bg fg)
  (colors-set! cursed
               (get-option attr)
               (get-color-option bg)
               (get-color-option fg)))

(define (init-colors!)
  (init-cursed! CURSED-CMDLINE
                'color-cmdline-attr
                'color-cmdline-bg
                'color-cmdline-fg)
  (init-cursed! CURSED-ERROR
                'color-cmdline-attr
                'color-cmdline-bg
                'color-error)
  (init-cursed! CURSED-INFO
                'color-cmdline-attr
                'color-cmdline-bg
                'color-cmdline-fg)
  (init-cursed! CURSED-SEPARATOR
                'color-win-attr
                'color-win-bg
                'color-separator)
  (init-cursed! CURSED-STATUSLINE
                'color-statusline-attr
                'color-statusline-bg
                'color-statusline-fg)
  (init-cursed! CURSED-TITLELINE
                'color-titleline-attr
                'color-titleline-bg
                'color-titleline-fg)
  (init-cursed! CURSED-WIN
                'color-win-attr
                'color-win-bg
                'color-win-fg)
  (init-cursed! CURSED-WIN-CUR
                'color-win-attr
                'color-win-bg
                'color-win-cur)
  (init-cursed! CURSED-WIN-CUR-SEL
                'color-win-cur-sel-attr
                'color-win-cur-sel-bg
                'color-win-cur-sel-fg)
  (init-cursed! CURSED-WIN-INACTIVE-CUR-SEL
                'color-win-inactive-cur-sel-attr
                'color-win-inactive-cur-sel-bg
                'color-win-inactive-cur-sel-fg)
  (init-cursed! CURSED-WIN-INACTIVE-SEL
                'color-win-inactive-sel-attr
                'color-win-inactive-sel-bg
                'color-win-inactive-sel-fg)
  (init-cursed! CURSED-WIN-SEL
                'color-win-sel-attr
                'color-win-sel-bg
                'color-win-sel-fg)
  (init-cursed! CURSED-WIN-TITLE
                'color-win-title-attr
                'color-win-title-bg
                'color-win-title-fg)
  (update-colors!)
  (cursed-set! CURSED-WIN))

(define (cursed-set! cursed)
  (bkgdset (COLOR_PAIR (cursed-pair cursed))))

(define (update-colors!)
  (define (*update-colors! i)
    (when (< i NR-CURSED)
      (init_pair (cursed-pair i) (cursed-fg i) (cursed-bg i))
      (*update-colors! (+ i 1))))
  (*update-colors! 0))

;; colors }}}

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
  (init-colors!)
  (alist-update! 'library
                 (make-list-window *artists*
                                   'library-changed)
                 *windows*)
  (alist-update! 'queue
                 (make-list-window *queue*
                                   'queue-changed
                                   (lambda (w)
                                     (scmus-play-track! (window-selected w))))
                 *windows*)
  (alist-update! 'status
                 (make-list-window *mpd-status* 'status-changed)
                 *windows*)
  (alist-update! 'error
                 (make-string-window *scmus-error* 'error-changed)
                 *windows*))

(define (exit-curses)
  (handle-exceptions exn
    (void)
    (endwin)))
