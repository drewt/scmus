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
         (uses scmus-client eval-mode command-line keys format ncurses
               option window search-view library-view options-view
               browser-view)
         (export *ui-initialized* *current-input-mode* *current-view*
                 simple-print-line format-print-line track-print-line
                 alist-print-line separator? view-window update-view!
                 current-window set-window! set-view! push! win-move!
                 win-bottom! win-top! win-add! win-remove! win-clear!
                 win-move-tracks! win-clear-marked! win-search!
                 win-search-next! win-search-prev! win-edit! make-view
                 register-event! curses-update cursor-on cursor-off
                 set-input-mode! handle-input init-curses exit-curses))

(define-constant CURSED-CMDLINE 1)
(define-constant CURSED-ERROR 2)
(define-constant CURSED-INFO 3)
(define-constant CURSED-STATUSLINE 4)
(define-constant CURSED-TITLELINE 5)
(define-constant CURSED-WIN 6)
(define-constant CURSED-WIN-CUR 7)
(define-constant CURSED-WIN-CUR-SEL 8)
(define-constant CURSED-WIN-SEL 9)
(define-constant CURSED-WIN-MARKED 10)
(define-constant CURSED-WIN-TITLE 11)
(define-constant NR-CURSED 11)

(define *ui-initialized* #f)
(define *current-input-mode* 'normal-mode)
(define *current-view* 'queue)
(define *current-editable* #f)
(define *editable-pos* #f)

(define *views* (map (lambda (x) (cons x #f)) *view-names*))

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

(define (win-add!)
  (case *current-view*
    ((library) (library-add-selected! (current-window)))
    ((search)  (search-add! (current-window)))
    ((browser) (browser-add-selected! (current-window)))))

(define (win-remove!)
  (case *current-view*
    ((queue) (let loop ((marked (sort (window-marked (current-window))
                                      >)))
               (unless (null? marked)
                 (scmus-delete! (car marked))
                 (loop (cdr marked))))
             (window-clear-marked! (current-window)))
    ((search) (search-remove! (current-window)))))

(define (win-clear!)
  (case *current-view*
    ((queue) (scmus-clear!))
    ((search) (search-clear! (current-window)))))

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
    ((search) (search-edit! (current-window)))
    ((options) (option-edit! (current-window)))))

;; user functions }}}
;; windows {{{

(define-record-type view
  (*make-view window title-fmt print-line cursed)
  view?
  (window *view-window *view-window-set!)
  (title-fmt view-title-fmt view-title-fmt-set!)
  (print-line *view-print-line)
  (cursed view-cursed-fn))

(define (make-view window title print-line #!optional (cursed generic-cursed-set!))
  (*make-view window (process-format title) print-line cursed))

(define (view-window view-name)
  (*view-window (alist-ref view-name *views*)))

(define (set-window! view-name window)
  (*view-window-set! (alist-ref view-name *views*) window))

(define (view-print-title! view)
  (cursed-set! CURSED-WIN-TITLE)
  (track-print-line 0 (view-title-fmt view) '() CURSED-WIN-TITLE))

(define (view-print-line! view row line-nr cursed)
  ((*view-print-line view) (*view-window view) row line-nr cursed))

(define (view-cursed-set! view row line-nr)
  ((view-cursed-fn view) (*view-window view) row line-nr))

(define (update-view! view-name)
  (if (current-view? view-name)
    (print-view! (alist-ref view-name *views*))))

(define (current-window)
  (view-window *current-view*))

(define (current-view? view-name)
  (eqv? view-name *current-view*))

(define (set-view! view-name)
  (when (memv view-name *view-names*)
    (set! *current-view* view-name)
    (window-changed! (view-window view-name))))

;; windows }}}
;; screen updates {{{

(define (print-view! view)
  (view-print-title! view)
  (let* ((window (*view-window view))
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
          (loop (if (null? rows) '() (cdr rows)) (- lines 1)))))))

(define (format-addstr! str cursed)
  (let loop ((str str))
    (let ((i (string-index str color-code?)))
      (if i
        (let ((code (ch->color-code (string-ref str i))))
          (addstr (string-take str i))
          ;(cursed-aux-set! code)
          (if (= code -2)
            (cursed-set! cursed)
            (cursed-temp-set! cursed code))
          (loop (substring/shared str (+ i 1))))
        (addstr str)))))

(define (track-print-line line fmt track cursed)
  (assert (integer? line) "track-print-line" line)
  (assert (list? fmt) "track-print-line" fmt)
  (assert (list? track) "track-print-line" track)
  (mvaddch line 0 #\space)
  (format-addstr! (scmus-format fmt (- (COLS) 2) track) cursed)
  (clrtoeol))

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
(define (alist-print-line window row line-nr cursed)
  (simple-print-line line-nr (car row))
  (mvaddstr line-nr (- (quotient (COLS) 2) 1)
            (string-truncate (format " ~a" (cdr row))
                             (- (COLS) 2))))

(define (list-window-print-row window row line-nr cursed)
  (simple-print-line line-nr row))

(define (separator? row)
  (and (pair? row) (eqv? (car row) 'separator)))

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

(define (update-current-line)
  (cursed-set! CURSED-TITLELINE)
  (track-print-line (- (LINES) 3)
                     (get-format 'format-current)
                     *current-track*
                     CURSED-TITLELINE))

(define (update-status-line)
  (cursed-set! CURSED-STATUSLINE)
  (track-print-line (- (LINES) 2)
                     (get-format 'format-status)
                     *current-track*
                     CURSED-STATUSLINE))

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
  (if *current-editable*
    (move (car *editable-pos*)
          (min (+ (cdr *editable-pos*)
                  (editable-cursor-pos *current-editable*))
               (- (COLS) 1)))))

(define (redraw-ui)
  (for-each (lambda (x) (window-nr-lines-set! (*view-window (cdr x))
                                              (- (LINES) 4)))
            *views*)
  (print-view! (alist-ref *current-view* *views*))
  (update-current)
  (update-status)
  (update-command-line))

;; screen updates }}}

(define (cursor-on)
  (curs_set 1))

(define (cursor-off)
  (curs_set 0))

(define (set-input-mode! mode #!optional (arg0 #f) (arg1 #f))
  (assert (symbol? mode) "set-input-mode!" mode)
  (assert (memv mode '(normal-mode edit-mode)) "set-input-mode!" mode)
  (case mode
    ((normal-mode) (enter-normal-mode))
    ((edit-mode)   (assert (editable? arg0) "set-input-mode!" arg0)
                   (assert (pair? arg1) "set-input-mode!" arg1)
                   (assert (and (integer? (car arg1)) (integer? (cdr arg1)))
                           "set-input-mode!" (car arg1) (cdr arg1))
                   (set! *current-editable* arg0)
                   (set! *editable-pos* arg1)
                   (cursor-on)
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
  (let-values (((ch rc) (get-char)))
    (cond
      ((= rc KEY_CODE_YES) (handle-key ch))
      ((= rc ERR) #f)
      (else (handle-char (integer->char ch))))))

;; colors {{{

(define (color->number color)
  (if (and (integer? color) (>= color -1) (< color 256))
    color
    (case color
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
      (else            #f))))

(define (safe-color->number color)
  (let ((n (color->number color)))
    (if (< n (COLORS))
      n
      -1)))

(define (attr->number attr)
  (if (integer? attr)
    attr
    (case attr
      ((default)    0)
      ((normal)     A_NORMAL)
      ((underline)  A_UNDERLINE)
      ((reverse)    A_REVERSE)
      ((blink)      A_BLINK)
      ((bold)       A_BOLD)
      ((dim)        A_DIM)
      ((altcharset) A_ALTCHARSET)
      ((invis)      A_INVIS)
      ((attributes) A_ATTRIBUTES)
      ((chartext)   A_CHARTEXT)
      ((color)      A_COLOR)
      ((standout)   A_STANDOUT)
      ((protect)    A_PROTECT)
      ((left)       A_LEFT)
      ((right)      A_RIGHT)
      ((low)        A_LOW)
      ((top)        A_TOP)
      ((vertical)   A_VERTICAL)
      (else         #f))))
 
(define *colors* (make-vector NR-CURSED))

(define (get-color-option name)
  (let ((option (get-option name)))
    (assert (list? option))
    (list (attr->number (car option))
          (safe-color->number (cadr option))
          (safe-color->number (caddr option)))))

(define (cursed-i cursed)
  (- cursed 1))

(define (cursed-attr cursed)
  (car (vector-ref *colors* (cursed-i cursed))))

(define (cursed-bg cursed)
  (cadr (vector-ref *colors* (cursed-i cursed))))

(define (cursed-fg cursed)
  (caddr (vector-ref *colors* (cursed-i cursed))))

(define (init-cursed! cursed color)
  (vector-set! *colors* (cursed-i cursed) (get-color-option color)))

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
  (cursed-set! CURSED-WIN))

(define (cursed-set! cursed)
  (bkgdset (bitwise-ior (COLOR_PAIR cursed)
                        (cursed-attr cursed)))
  cursed)

(define (find-pair fg bg)
  (let loop ((i 1))
    (if (< i (COLOR_PAIRS))
      (let-values (((p-fg p-bg) (pair_content i)))
        (if (and (= p-fg fg)
                 (= p-bg bg))
          i
          (loop (+ i 1))))
      #f)))

(define cursed-temp-set!
  (let ((next (+ NR-CURSED 1)))
    (define (cursed-set! pair attr)
      (bkgdset (bitwise-ior (COLOR_PAIR pair) attr)))
    (lambda (base #!optional (fg (cursed-fg base))
                             (bg (cursed-bg base))
                             (attr (cursed-attr base)))
      (cond
        ((or (>= fg (COLORS)) (>= bg (COLORS))) (void))
        ((find-pair fg bg) => (lambda (x) (cursed-set! x attr)))
        (else
          (let ((this next))
            (set! next (if (< next (- (COLOR_PAIRS) 1))
                         (+ next 1)
                         (+ NR-CURSED 1)))
            (init_pair this fg bg)
            (cursed-set! this attr)))))))

;; colors }}}

(define (view-update-fn view-name)
  (lambda () (update-view! view-name)))

(define (view-update-data-fn view-name)
  (lambda ()
    (window-data-len-update! (view-window view-name))
    (update-view! view-name)))

(define *events* '())
(define *event-handlers*
  (list (cons 'command-line-changed update-command-line)
        (cons 'status-changed update-status)
        (cons 'current-line-changed update-current)
        (cons 'library-changed (view-update-fn 'library))
        (cons 'library-data-changed (view-update-data-fn 'library))
        (cons 'queue-changed (view-update-fn 'queue))
        (cons 'queue-data-changed (view-update-data-fn 'queue))
        (cons 'search-changed (view-update-fn 'search))
        (cons 'browser-changed (view-update-fn 'browser))
        (cons 'browser-data-changed (view-update-data-fn 'browser))
        (cons 'error-changed (view-update-data-fn 'error))
        (cons 'option-data-changed (lambda () (update-options-data (view-window 'options))))
        (cons 'option-changed (view-update-fn 'options))
        (cons 'color-changed update-colors!)
        (cons 'format-changed redraw-ui)
        (cons 'db-changed update-db)))

(define (register-event! event)
  (assert (symbol? event) "register-event!" event)
  (set! *events* (cons event *events*)))

(define (curses-update)
  (for-each (lambda (x)
              ((alist-ref x *event-handlers*)))
            *events*)
  (update-cursor)
  (set! *events* '()))

(define (make-queue-view)
  (make-view (make-window #f
                          (lambda (w) *queue*)
                          (lambda (w) (register-event! 'queue-changed))
                          (lambda (w) (scmus-play-track! (window-selected w)))
                          void
                          track-match)
             "Queue - ~{queue-length} tracks"
             (lambda (window track line-nr cursed)
               (track-print-line line-nr (get-format 'format-queue) track cursed))
             trackwin-cursed-set!))

(define (make-status-view)
  (make-view (make-window #f
                          (lambda (w) *mpd-status*)
                          (lambda (w) (register-event! 'status-changed))
                          void
                          void
                          (lambda (e q) #f))
             "MPD Status"
             alist-print-line))

(define (make-error-view)
  (make-view (make-window #f
                          (lambda (w) (string-split-lines *scmus-error*))
                          (lambda (w) (register-event! 'error-changed))
                          void
                          void
                          (lambda (e q) #f))
             "Error"
             list-window-print-row))

(define (init-views!)
  (alist-update! 'library (make-library-view) *views*)
  (alist-update! 'queue (make-queue-view) *views*)
  (alist-update! 'search (make-search-view) *views*)
  (alist-update! 'browser (make-browser-view) *views*)
  (alist-update! 'status (make-status-view) *views*)
  (alist-update! 'error (make-error-view) *views*)
  (alist-update! 'options (make-options-view) *views*))

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
  (handle-exceptions exn
    (void)
    (endwin)))
