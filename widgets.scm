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

(module scmus.widgets *
  (import coops
          coops-utils
          vector-lib
          scmus.base
          scmus.format
          scmus.track
          scmus.tui)

  ;; special verbs {{{
  ;;
  ;; Generic actions that widgets can implement if they want to. By default, the
  ;; message is passed up to the parent.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-method (widget-activate (widget <widget>))
    (when (widget-parent widget)
      (widget-activate (widget-parent widget))))

  (define-method (widget-deactivate (widget <widget>))
    (when (widget-parent widget)
      (widget-deactivate (widget-parent widget))))

  (define-method (widget-edit (widget <widget>))
    (when (widget-parent widget)
      (widget-edit (widget-parent widget))))

  (define-method (widget-add (widget <widget>))
    (when (widget-parent widget)
      (widget-add (widget-parent widget))))

  (define-method (widget-remove (widget <widget>))
    (when (widget-parent widget)
      (widget-remove (widget-parent widget))))

  (define-method (widget-clear (widget <widget>))
    (when (widget-parent widget)
      (widget-clear (widget-parent widget))))

  (define-method (widget-move (widget <widget>) n relative?)
    (when (widget-parent widget)
      (widget-move (widget-parent widget) n relative?)))

  (define-method (widget-move-top (widget <widget>))
    (when (widget-parent widget)
      (widget-move-top (widget-parent widget))))

  (define-method (widget-move-bottom (widget <widget>))
    (when (widget-parent widget)
      (widget-move-bottom (widget-parent widget))))

  (define-method (widget-mark (widget <widget>))
    (when (widget-parent widget)
      (widget-mark (widget-parent widget))))
  
  (define-method (widget-unmark (widget <widget>))
    (when (widget-parent widget)
      (widget-unmark (widget-parent widget))))

  (define-method (widget-toggle-mark (widget <widget>))
    (when (widget-parent widget)
      (widget-toggle-mark (widget-parent widget))))

  (define-method (widget-clear-marked (widget <widget>))
    (when (widget-parent widget)
      (widget-clear-marked (widget-parent widget))))

  (define-method (widget-paste (widget <widget>) before?)
    (when (widget-parent widget)
      (widget-paste (widget-parent widget) before?)))

  (define-method (widget-search (widget <widget>) query backward?)
    (when (widget-parent widget)
      (widget-search (widget-parent widget) query backward?)))

  (define-method (widget-match (widget <widget>) query)
    #f)

  ;; special verbs }}}
  ;; <format-text> {{{

  (define-class <format-text> (<textual>)
    ((format initform: (process-format "")
             accessor: format-text-format)
     (data   initform: '()
             accessor: format-text-data)))

  (define (make-format-text format data . kwargs)
    (apply make <format-text> 'format (if (string? format)
                                        (process-format format)
                                        format)
                              'data data kwargs))

  (define-method ((setter format-text-format) after: (text <format-text>) fmt)
    (widget-damaged! text))

  (define-method ((setter format-text-data) after: (text <format-text>) data)
    (widget-damaged! text))

  (define-method (widget-size (text <format-text>) available-cols available-rows)
    (values available-cols (min available-rows 1)))

  (define-method (text-text (text <format-text>))
    (list (scmus-format (format-text-format text) (widget-cols text) (format-text-data text))))
  
  ;; <format-text> }}}
  ;; <window> {{{
  ;;
  ;; A window is a list with a visible section and a cursor.  The visible
  ;; section follows the cursor.
  ;;
  ;;              ,+- - - - - - - - - - - -+ <--- 0
  ;;             / |                       |
  ;;            |  +- - - - - - - - - - - -+
  ;;            |  |                       |
  ;;            |  +=======================+,<--- top-pos
  ;;            |  |                       | \
  ;;            |  +-----------------------+<-|-- sel-pos
  ;; data-len -<   |#######################|  |
  ;;            |  +-----------------------+   >- nr-lines
  ;;            |  |                       |  |
  ;;            |  +-----------------------+  |
  ;;            |  |                       | /
  ;;            |  +=======================+`
  ;;             \ |                       |
  ;;              `+- - - - - - - - - - - -+
  ;;
  ;; A window can be searched.  This works by calling the function in the @match
  ;; slot on each row of the window; if @match returns true, then the row is
  ;; considered to be a match for the query.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <window> (<container>)
    ((data       initform: #()
                 reader:   *window-data)
     (top-pos    initform: 0
                 accessor: window-top-pos)
     (sel-pos    initform: 0
                 accessor: window-sel-pos)
     (marked     initform: '()
                 ; writer below
                 reader:   *window-marked)
     (h-border   initform: 1
                 accessor: window-h-border)
     (cursed-fn  initform: (lambda (w r l) #f)
                 reader:   *window-cursed)
     (format     initform: #f
                 reader:   window-format)))

  (define-method (initialize-instance (window <window>))
    (call-next-method)
    ; allow giving window-data as a list
    (cond
      ((vector? (*window-data window)) (void))
      ((list? (*window-data window))
        (set! (slot-value window 'data) (list->vector (*window-data window))))
      (else (assert #f "initialize-instance" window)))
    ; allow giving a format string directly instead of a procedure
    (let ((format (window-format window)))
      (if (and format (not (procedure? format)))
        (set! (slot-value window 'format) (lambda (tag) format)))))

  (define-method (window-data (window <window>))
    (vector->list (slot-value window 'data)))

  (define-method ((setter window-data) (window <window>) data)
    ; convert data to vector
    (let* ((data (cond
                   ((vector? data) data)
                   ((list? data)   (list->vector data))
                   (else           (assert #f "(setter window-data)" window data))))
           (len  (vector-length data)))
      (set! (slot-value window 'data) data)
      ; set parent on children
      (vector-for-each (lambda (i row)
                         (set! (widget-parent row) window))
                       data)
      ; update pointers into data
      (when (>= (window-sel-pos window) len)
        (set! (window-sel-pos window) (max 0 (- len 1))))
      (when (>= (window-top-pos window) len)
        (set! (window-top-pos window) (max 0 (- len 1))))
      (widget-damaged! window)))

  (define-method (window-length (window <window>))
    (vector-length (*window-data window)))

  (define-method ((setter *window-marked) (window <window>) marked)
    (set! (slot-value window 'marked) marked)
    (widget-damaged! window))

  ;; When the number of visible lines changes, the cursor may go off-screen;
  ;; if so, we adjust TOP-POS so that the cursor remains on-screen.
  (define-method ((setter widget-rows) (window <window>) rows)
    (let ((top-pos (window-top-pos window))
          (sel-pos (window-sel-pos window)))
      (when (<= rows (- sel-pos top-pos))
        (set! (window-top-pos window)
              (+ top-pos
                 (- (widget-rows window) rows)))))
    (call-next-method))

  (define (make-window . args)
    (apply make <window> args))

  (define-method (window-empty? (window <window>))
    (zero? (window-length window)))

  (: window-sel-offset (window -> fixnum))
  (define (window-sel-offset window)
    (- (window-sel-pos window)
       (window-top-pos window)))

  (: window-selected (window -> *))
  (define (window-selected window)
    (assert (> (window-length window) (window-sel-pos window))
            "window-selected"
            (window-length window)
            (window-sel-pos window))
    (vector-ref (*window-data window) (window-sel-pos window)))

  (: window-all-selected (window -> list))
  (define (window-all-selected window)
    (if (window-empty? window)
      '()
      (let ((data   (*window-data window))
            (marked (window-marked window)))
        (map (lambda (x) (vector-ref data x)) marked))))

  ;; XXX: selected row counts as marked
  (: window-marked (window -> list))
  (define (window-marked window)
    (if (window-empty? window)
      '()
      (let ((sel-pos (window-sel-pos window))
            (marked (*window-marked window)))
        (if (member sel-pos marked)
          marked
          (cons sel-pos marked)))))

  (define-method (widget-mark (window <window>))
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (unless (or (window-empty? window)
                  (member sel-pos marked))
        (set! (*window-marked window) (cons sel-pos marked)))))

  (define-method (widget-unmark (window <window>))
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (if (and (not (window-empty? window))
               (member sel-pos marked))
        (set! (*window-marked window) (remove (lambda (x) (= x sel-pos)) marked)))))

  (define-method (widget-toggle-mark (window <window>))
    (let ((sel-pos (window-sel-pos window))
          (marked (*window-marked window)))
      (when (not (window-empty? window))
        (if (member sel-pos marked)
          (set! (*window-marked window) (remove (lambda (x) (= x sel-pos)) marked))
          (set! (*window-marked window) (cons sel-pos marked))))))

  (define-method (widget-clear-marked (window <window>))
    (set! (*window-marked window) '())
    (widget-damaged! window))

  (: window-cursed (window * fixnum -> undefined))
  (define (window-cursed window row line-nr)
    ((*window-cursed window) window row line-nr))

  (: window-move-down! (window fixnum -> undefined))
  (define (window-move-down! window n)
    (let* ((top-pos  (window-top-pos window))
           (sel-pos  (window-sel-pos window))
           (data-len (window-length window))
           (nr-lines (widget-rows window))
           (can-move (max 0 (min n (- data-len sel-pos 1))))
           (scroll   (max 0 (+ 1 (- (+ sel-pos can-move)
                                    (+ top-pos nr-lines))))))
      (set! (window-top-pos window) (+ top-pos scroll))
      (set! (window-sel-pos window) (+ sel-pos can-move))
      (widget-damaged! window)))

  (: window-move-up! (window fixnum -> undefined))
  (define (window-move-up! window n)
    (let* ((top-pos (window-top-pos window))
           (sel-pos (window-sel-pos window))
           (can-move (min n sel-pos))
           (scroll (max 0 (- can-move
                             (- sel-pos top-pos)))))
      (set! (window-top-pos window) (- top-pos scroll))
      (set! (window-sel-pos window) (- sel-pos can-move))
      (widget-damaged! window)))

  (: window-move-top! (window -> undefined))
  (define (window-move-top! window)
    (window-select! window 0)
    (void))

  (: window-move-bottom! (window -> undefined))
  (define (window-move-bottom! window)
    (window-select! window (- (window-length window) 1))
    (void))

  (define (window-move-cursor! window n #!optional relative)
    (let ((nr-lines (if relative
                      (integer-scale (widget-rows window) n)
                      n)))
      (if (> nr-lines 0)
        (window-move-down! window nr-lines)
        (window-move-up! window (abs nr-lines))))
    (void))

  (define-method (widget-move (window <window>) n relative?)
    (let ((nr-lines (if relative?
                      (integer-scale (widget-rows window) n)
                      n)))
      (if (> nr-lines 0)
        (window-move-down! window nr-lines)
        (window-move-up! window (abs nr-lines))))
    (void))

  (define-method (widget-move-top (window <window>))
    (window-select! window 0)
    (void))

  (define-method (widget-move-bottom (window <window>))
    (window-select! window (- (window-length window) 1))
    (void))

  (: window-select! (window fixnum -> undefined))
  (define (window-select! window i)
    (let* ((sel-pos (window-sel-pos window))
           (diff (- sel-pos i)))
      (cond
        ((> diff 0) (window-move-up! window diff))
        ((< diff 0) (window-move-down! window (abs diff))))))

  ;; search {{{
  (define-method (widget-search (window <window>) query backward?)
    (unless (window-empty? window)
      (let ((i (if backward?
                 (window-search-backward window query)
                 (window-search-forward window query))))
        (when i (window-select! window i)))))

  (define (window-search-forward window query)
    (define (match-fun row)
      (widget-match row query))
    (or (window-index window match-fun (+ 1 (window-sel-pos window)))
        (window-index window match-fun 0 (+ 1 (window-sel-pos window)))))

  (define (window-search-backward window query)
    (define (match-fun row)
      (widget-match row query))
    (or (window-index-right window match-fun 0 (window-sel-pos window))
        (window-index-right window match-fun (window-sel-pos window))))

  (define (window-index window pred? #!optional (start 0) (end (window-length window)))
    (let ((data (*window-data window)))
      (let loop ((i start))
        (cond
          ((>= i end) #f)
          ((pred? (vector-ref data i)) i)
          (else (loop (+ i 1)))))))

  (define (window-index-right window pred? #!optional (start 0) (end (window-length window)))
    (let ((data (*window-data window)))
      (let loop ((i (- end 1)))
        (cond
          ((< i start) #f)
          ((pred? (vector-ref data i)) i)
          (else (loop (- i 1)))))))
  ;; search }}}

  (define-method (container-children (window <window>))
    (vector->list (*window-data window)))

  (define-method (widget-focus (window <window>))
    (widget-focus (window-selected window)))

  ;; Even though <window>s are <container>s, we still implement PRINT-WIDGET!
  ;; so that we can use WITH-CURSED per-line.
  (define-method (print-widget! (window <window>) x y cols rows)
    (let ((data     (*window-data window))
          (top-pos  (window-top-pos window))
          (data-len (window-length window)))
      (let loop ((line top-pos))
        (when (and (< line data-len)
                   (< line (+ top-pos rows)))
          (let ((row    (vector-ref data line))
                (row-nr (- line top-pos)))
            (with-cursed (or (widget-cursed row)
                             (window-cursed window row (+ y row-nr)))
              (print-widget! row
                             (+ x (window-h-border window))
                             (+ y row-nr)
                             (- cols (* 2 (window-h-border window)))
                             1)))
            (loop (+ line 1))))))

  ;; <window> }}}
  ;; <window-row> {{{

  (define-class <window-row> (<textual>)
    ((data   reader: window-row-data)
     (type   reader: window-row-type)
     (format reader: window-row-format)))

  (define (make-window-row data type format)
    (make <window-row> 'data data 'type type 'format format))

  (define-method (text-text (widget <window-row>))
    (list (scmus-format ((window-row-format widget) widget)
                        (widget-cols widget)
                        (window-row-data widget))))

  (define-method (widget-match (row <window-row>) query)
    (let ((data (window-row-data row)))
      (case (window-row-type row)
        ((directory playlist artist album)
          (substring-match (cdar data) query))
        ((file)
          (track-match data query))
        ((metadata)
          (or (substring-match (format "~a" (cdar data)) query)
              (substring-match (format "~a" (cdadr data)) query)))
        (else #f))))

  ;; <window-row> }}}
  ;; <window-separator> {{{

  (define-class <window-separator> (<separator>)
    ((text   initform: ""
             accessor: window-separator-text)
     (indent initform: 0
             accessor: window-separator-indent)))

  (define-method (print-widget! (widget <window-separator>) x y cols rows)
    (let ((text (string-append (make-string (window-separator-indent widget)
                                            (separator-char widget))
                               (window-separator-text widget))))
      (print-line! text x y cols (separator-char widget))))

  ;; <window-separator> }}}
  ;; <scheme-text> {{{
  ;; Text widget that displays a stored scheme expression.

  (define-class <scheme-text> (<textual>)
    ((expr   accessor: scheme-text-expr)
     (format accessor: scheme-text-format
             initform: "~s")))

  (define-method (text-text (widget <scheme-text>))
    (list (format #f (scheme-text-format widget) (scheme-text-expr widget))))

  (define (make-scheme-text expr . args)
    (apply make <scheme-text> 'expr expr args))

  ;; <scheme-text> }}}
 )
