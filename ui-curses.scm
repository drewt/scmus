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

(require-extension ncurses srfi-13)

(declare (unit ui-curses)
         (uses scmus-client eval-mode command-line keys format
               option search window library-view)
         (export *ui-initialized* *current-input-mode* *current-view*
                 current-window set-window! set-view! push! win-move!
                 win-bottom! win-top! win-add! win-remove! win-clear!
                 win-move-tracks! win-clear-marked! win-search!
                 win-search-next! win-search-prev! win-edit! register-event!
                 curses-update cursor-on cursor-off set-input-mode!
                 handle-input init-curses exit-curses))

;;; definitions missing from the ncurses egg
(define bkgdset
  (foreign-lambda* void ((unsigned-long a0)) "bkgdset(a0);"))
(define use_default_colors
  (foreign-lambda* integer () "return(use_default_colors());"))

(define (get-wch)
  (let ((*get-wch (foreign-lambda integer "get_wch" (c-pointer unsigned-int))))
    (let-location ((ch unsigned-int))
      (let ((rc (*get-wch (location ch))))
        (values ch rc)))))

(define-constant CURSED-CMDLINE 0)
(define-constant CURSED-ERROR 1)
(define-constant CURSED-INFO 2)
(define-constant CURSED-STATUSLINE 3)
(define-constant CURSED-TITLELINE 4)
(define-constant CURSED-WIN 5)
(define-constant CURSED-WIN-CUR 6)
(define-constant CURSED-WIN-CUR-SEL 7)
(define-constant CURSED-WIN-SEL 8)
(define-constant CURSED-WIN-MARKED 9)
(define-constant CURSED-WIN-TITLE 10)
(define-constant NR-CURSED 11)

(define *ui-initialized* #f)
(define *current-input-mode* 'normal-mode)
(define *current-view* 'queue)
(define *current-editable* #f)
(define *editable-pos* #f)

(define *windows* (map (lambda (x) (cons x #f)) *views*))

;; user functions {{{

(define (push! str)
  (enter-eval-mode)
  (command-line-text-set! str))

(define (win-move! nr #!optional (relative #f))
  (assert (integer? nr))
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

(define (win-add! #!optional (view 'queue) (pos #f))
  (case *current-view*
    ((library)
      (case view
        ((queue) (library-add-selected! (alist-ref 'library *windows*)))))
    ((search) (search-add! (alist-ref 'search *windows*)))))

(define (win-remove!)
  (case *current-view*
    ((queue) (let loop ((marked (sort (window-marked (current-window))
                                      >)))
               (unless (null? marked)
                 (scmus-delete! (car marked))
                 (loop (cdr marked))))
             (window-clear-marked! (current-window)))
    ((search) (search-remove! (alist-ref 'search *windows*)))))

(define (win-clear!)
  (case *current-view*
    ((queue) (scmus-clear!))
    ((search) (search-clear! (alist-ref 'search *windows*)))))

(define (win-move-tracks!)
  (case *current-view*
    ((queue) (let loop ((marked (sort (*window-marked (current-window))
                                       <))
                        (pos (window-sel-pos (current-window))))
               (unless (null? marked)
                 (scmus-move! (car marked) pos)
                 (loop (cdr marked) (+ pos 1))))
             (window-clear-marked! (current-window)))))

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

(define (win-edit!)
  (case *current-view*
    ((search) (search-edit! (alist-ref 'search *windows*)))))

;; user functions }}}
;; windows {{{

;; Convenient interface to make-window.  Can only be used *after* ncurses is
;; initialized.
(define-syntax make-global-window
  (syntax-rules ()
    ((make-global-window get-data changed-event activate deactivate match)
      (make-window #f
                   get-data
                   (lambda (w) (register-event! changed-event))
                   activate
                   deactivate
                   match))))

;; make-window for global lists.  Each member of the list becomes a row in the
;; window.
(define-syntax make-global-list-window
  (syntax-rules ()
    ((make-global-list-window lst changed-event activate deactivate match)
      (make-global-window (lambda (w) lst)
                          changed-event
                          activate
                          deactivate
                          match))
    ((make-global-list-window lst changed-event activate deactivate)
      (make-global-list-window lst changed-event activate deactivate
                               (lambda (e q) #f)))
    ((make-global-list-window lst changed-event activate)
      (make-global-list-window lst changed-event activate void
                               (lambda (e q) #f)))
    ((make-global-list-window lst changed-event)
      (make-global-list-window lst changed-event void void
                               (lambda (e q) #f)))))

;; make-window for global strings.  Each line in the string becomes a row in
;; the window.
(define-syntax make-global-string-window
  (syntax-rules ()
     ((make-global-string-window str changed-event activate deactivate match)
       (make-global-window (lambda (w) (string-split-lines str))
                           changed-event
                           activate
                           deactivate
                           match))
     ((make-global-string-window str changed-event activate deactivate)
       (make-global-string-window str changed-event activate deactivate
                                  string-contains-ci))
     ((make-global-string-window str changed-event activate)
       (make-global-string-window str changed-event activate void
                                  string-contains-ci))
     ((make-global-global-string-window str changed-event)
       (make-global-string-window str changed-event void void
                                  string-contains-ci))))

(define (current-window)
  (alist-ref *current-view* *windows*))

(define (set-window! name window)
  (alist-update! name window *windows*))

(define (current-view? view)
  (eqv? view *current-view*))

(define (set-view! view)
  (when (memv view *views*)
    (set! *current-view* view)
    (window-changed! (alist-ref view *windows*))))

(define (option-activate! window)
  (let* ((selected (window-selected window))
         (name (car selected))
         (option (cdr selected)))
    (push! (format "(set-option! '~a ~a)" name (option-string option)))))

;; windows }}}
;; screen updates {{{

(define (track-print-line line fmt track)
  (assert (integer? line))
  (assert (list? fmt))
  (assert (list? track))
  (let* ((left-right (scmus-format fmt (- (COLS) 2) track))
         (left (car left-right))
         (right (cdr left-right)))
    (mvaddch line 0 #\space)
    (addstr left)
    (clrtoeol)
    (mvaddstr line
              (- (COLS) (string-length right) 1)
              right)))

;; Calls print-fn with window, row item, and line number for each row in
;; window.
(define (print-window window print-fn #!optional (cursed-fn generic-cursed-set!))
  (let ((nr-lines (window-nr-lines window)))
    (let loop ((rows (window-top window)) (lines nr-lines))
      (when (> lines 0)
        (let ((line-nr (- nr-lines (- lines 1))))
          (if (null? rows)
            (begin (cursed-set! CURSED-WIN)
                   (move line-nr 0)
                   (clrtoeol))
            (begin (cursed-fn window (car rows) line-nr)
                   (print-fn window (car rows) line-nr)))
          (loop (if (null? rows) '() (cdr rows)) (- lines 1)))))))

;; Prints a window title from the given format.
(define (print-window-title fmt)
  (cursed-set! CURSED-WIN-TITLE)
  (track-print-line 0 fmt '()))

(define (simple-print-line line-nr str)
  (mvaddstr line-nr 0
            (string-truncate (format " ~a" str)
                             (- (COLS) 2)))
  (clrtoeol))

(define (format-print-line line-nr fmt . args)
  (mvaddstr line-nr 0
            (string-truncate (apply format fmt args)
                             (- (COLS) 2)))
  (clrtoeol))

;; Generic function to print an alist entry, for use with print-window.
(define (alist-window-print-row window row line-nr)
  (simple-print-line line-nr (car row))
  (mvaddstr line-nr (- (quotient (COLS) 2) 1)
            (string-truncate (format " ~a" (cdr row))
                             (- (COLS) 2))))

(define (list-window-print-row window row line-nr)
  (simple-print-line line-nr row))

(define (library-window-print-row window row line-nr)
  (case (car row)
    ((separator) (simple-print-line line-nr (cdr row)))
    ((playlist) (simple-print-line line-nr (cdr row)))
    ((artist) (format-print-line line-nr " [~a]" (cdr row)))
    ((album) (simple-print-line line-nr (cdr row)))
    ((track) (track-print-line line-nr (get-option 'format-library) (cdr row)))
    ((metadata) (alist-window-print-row window (cdr row) line-nr))))

(define (separator? row)
  (and (pair? row) (eqv? (car row) 'separator)))

(define (search-window-print-row window row line-nr)
  (cond
    ((editable? row)
      (format-print-line line-nr " * ~a" (editable-text row)))
    ((separator? row) (move line-nr 0) (clrtoeol))
    (else
      (track-print-line line-nr (get-option 'format-library) row))))

(define (options-window-print-row window row line-nr)
  (alist-window-print-row window
                          (cons (car row) (option-string (cdr row)))
                          line-nr))

;; Generates a function to call cursed-set! with the appropriate value given
;; a window, row, and line number.
(define (win-cursed-fn current?)
  (lambda (window row line-nr)
    (let* ((current (current? row))
           (row-pos (+ (window-top-pos window) (- line-nr 1)))
           (selected (= row-pos (window-sel-pos window)))
           (marked (member row-pos (window-marked window))))
      (cursed-set!
        (cond
          ((separator? row)       CURSED-WIN-TITLE)
          ((eqv? row 'separator)  CURSED-WIN-TITLE)
          ((and current selected) CURSED-WIN-CUR-SEL)
          (current                CURSED-WIN-CUR)
          (selected               CURSED-WIN-SEL)
          (marked                 CURSED-WIN-MARKED)
          (else                   CURSED-WIN))))))

;; cursed-set! suitable for any window (CURSED-CUR[-SEL] is never chosen)
(define generic-cursed-set! (win-cursed-fn (lambda (x) #f)))

;; cursed-set! for track windows (e.g. queue)
(define trackwin-cursed-set! (win-cursed-fn current-track?))

(define (update-track-window window title-fmt track-fmt)
  (print-window-title title-fmt)
  (print-window window
                (lambda (window track line-nr)
                  (track-print-line line-nr track-fmt track))
                trackwin-cursed-set!))

(define (update-library-window)
  (when (current-view? 'library)
    (print-window-title (process-format (string->list "Library")))
    (print-window (alist-ref 'library *windows*)
                  library-window-print-row)))

(define (update-library-data)
  (window-data-len-update! (alist-ref 'library *windows*))
  (redraw-ui)
  (update-library-window))

(define (update-queue-window)
  (when (current-view? 'queue)
    (update-track-window (alist-ref 'queue *windows*)
                         (get-option 'format-queue-title)
                         (get-option 'format-queue))))

(define (update-queue-data)
  (window-data-len-update! (alist-ref 'queue *windows*))
  (update-queue-window))

(define (update-search-window)
  (when (current-view? 'search)
    (print-window-title (process-format (string->list "Search")))
    (print-window (alist-ref 'search *windows*)
                  search-window-print-row)))

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

(define (update-options-window)
  (when (current-view? 'options)
    (print-window-title (process-format (string->list "Options")))
    (print-window (alist-ref 'options *windows*)
                  options-window-print-row)))

(define (update-current-line)
  (cursed-set! CURSED-TITLELINE)
  (track-print-line (- (LINES) 3)
                     (get-option 'format-current)
                     *current-track*))

(define (update-status-line)
  (cursed-set! CURSED-STATUSLINE)
  (track-print-line (- (LINES) 2)
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
  (move (- (LINES) 1) 0)
  (clrtoeol)
  (addch (case (command-line-mode)
           ((normal-mode) #\space)
           ((eval-mode)   #\:)
           ((search-mode) #\/)))
  (addstr (string-truncate (command-line-text)
                           (- (COLS) 2))))

(define (update-cursor)
  (if *current-editable*
    (move (car *editable-pos*) (+ (cdr *editable-pos*)
                                  (editable-cursor-pos *current-editable*)))))

(define (redraw-ui)
  (for-each (lambda (x) (window-nr-lines-set! (cdr x) (- (LINES) 4)))
            *windows*)
  (update-library-window)
  (update-queue-window)
  (update-search-window)
  (update-error)
  (update-current)
  (update-status)
  (update-options-window)
  (update-command-line))

;; screen updates }}}

(define (cursor-on)
  (curs_set 1))

(define (cursor-off)
  (curs_set 0))

(define (set-input-mode! mode #!optional (arg0 #f) (arg1 #f))
  (assert (symbol? mode))
  (assert (memv mode '(normal-mode edit-mode)))
  (case mode
    ((normal-mode) (enter-normal-mode))
    ((edit-mode)   (assert (editable? arg0))
                   (assert (pair? arg1))
                   (assert (and (integer? (car arg1)) (integer? (cdr arg1))))
                   (set! *current-editable* arg0)
                   (set! *editable-pos* arg1)
                   (editable-init arg0)))
  (set! *current-input-mode* mode))

(define (handle-key key)
  (cond
    ((= key KEY_RESIZE) (redraw-ui))
    (else
      (case *current-input-mode*
        ((normal-mode) (normal-mode-key key))
        ((edit-mode)   (editable-key *current-editable* key))))))

(define (handle-char ch)
  (case *current-input-mode*
    ((normal-mode) (normal-mode-char ch))
    ((edit-mode)   (editable-char *current-editable* ch))))

(define (handle-input)
  (let-values (((ch rc) (get-wch)))
    (cond
      ((= rc KEY_CODE_YES) (handle-key ch))
      ((= rc ERR) #f)
      (else (handle-char (integer->char ch))))))

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

(define (update-colors!)
  (define (*update-colors!)
    (let loop ((i 0))
      (when (< i NR-CURSED)
        (init_pair (cursed-pair i) (cursed-fg i) (cursed-bg i))
        (loop (+ i 1)))))
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
                'color-info)
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
  (init-cursed! CURSED-WIN-SEL
                'color-win-sel-attr
                'color-win-sel-bg
                'color-win-sel-fg)
  (init-cursed! CURSED-WIN-MARKED
                'color-win-marked-attr
                'color-win-marked-bg
                'color-win-marked-fg)
  (init-cursed! CURSED-WIN-TITLE
                'color-win-title-attr
                'color-win-title-bg
                'color-win-title-fg)
  (*update-colors!)
  (cursed-set! CURSED-WIN))

(define (cursed-set! cursed)
  (bkgdset (COLOR_PAIR (cursed-pair cursed))))

;; colors }}}

(define *events* '())
(define *event-handlers*
  (list (cons 'command-line-changed update-command-line)
        (cons 'status-changed update-status)
        (cons 'current-line-changed update-current)
        (cons 'library-changed update-library-window)
        (cons 'library-data-changed update-library-data)
        (cons 'queue-changed update-queue-window)
        (cons 'queue-data-changed update-queue-data)
        (cons 'search-changed update-search-window)
        (cons 'error-changed update-error)
        (cons 'option-changed update-options-window)
        (cons 'color-changed update-colors!)
        (cons 'format-changed redraw-ui)))

(define (register-event! event)
  (assert (symbol? event))
  (set! *events* (cons event *events*)))

(define (curses-update)
  (for-each (lambda (x)
              ((alist-ref x *event-handlers*)))
            *events*)
  (update-cursor)
  (set! *events* '()))

(define (init-windows!)
  (alist-update! 'library (make-library-window) *windows*)
  (alist-update! 'queue
                 (make-global-list-window *queue*
                                          'queue-changed
                                          (lambda (w)
                                            (scmus-play-track!
                                              (window-selected w)))
                                          void
                                          track-match)
                 *windows*)
  (alist-update! 'search (make-search-window) *windows*)
  (alist-update! 'status
                 (make-global-list-window *mpd-status* 'status-changed)
                 *windows*)
  (alist-update! 'error
                 (make-global-string-window *scmus-error* 'error-changed)
                 *windows*)
  (alist-update! 'options
                 (make-global-list-window *options*
                                          'option-changed
                                          option-activate!)
                 *windows*))

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
  (init-windows!)
  (redraw-ui))

(define (exit-curses)
  (handle-exceptions exn
    (void)
    (endwin)))
