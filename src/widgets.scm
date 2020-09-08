;;
;; Copyright 2014-2020 Drew Thoreson
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

(module (scmus widgets) *
  (import coops
          coops-utils
          vector-lib
          (drewt ncurses)
          (scmus base)
          (scmus format)
          (scmus option)
          (scmus track)
          (scmus tui))

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

  (define-method (widget-add (widget <widget>) dst)
    (when (widget-parent widget)
      (widget-add (widget-parent widget) dst)))

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

  (define-method (widget-data (widget <widget>))
    (if (widget-parent widget)
      (widget-data (widget-parent widget))
      #f))

  ;; special verbs }}}
  ;; <format-text> {{{

  (define-class <format-text> (<textual>)
    ((format initform: (compile-format-string "")
             accessor: format-text-format)
     (data   initform: '()
             accessor: format-text-data)))

  (define (make-format-text format data . kwargs)
    (apply make <format-text> 'format (if (string? format)
                                        (compile-format-string format)
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

  (define-class <window> (<list-box>)
    ((marked initform: '()
             accessor: *window-marked)))

  (define-method ((setter *window-marked) after: (window <window>) marked)
    (widget-damaged! window))

  (define (window-selected window)
    (if (list-box-empty? window)
      '()
      (let ((data   (list-box-data window)))
        (map (lambda (x) (vector-ref data x))
             (window-marked window)))))

  (define (window-marked window)
    (if (list-box-empty? window)
      '()
      (sort (lset-adjoin = (*window-marked window)
                           (list-box-sel-pos window))
            <)))

  (define-method (widget-mark (window <window>))
    (unless (list-box-empty? window)
      (set! (*window-marked window) (lset-adjoin = (*window-marked window)
                                                    (list-box-sel-pos window)))))

  (define-method (widget-unmark (window <window>))
    (unless (list-box-empty? window)
      (set! (*window-marked window) (lset-difference = (*window-marked window)
                                                        (list (list-box-sel-pos window))))))

  (define-method (widget-toggle-mark (window <window>))
    (unless (list-box-empty? window)
      (let ((marked  (*window-marked window))
            (sel-pos (list-box-sel-pos window)))
        (if (memq sel-pos marked)
          (set! (*window-marked window) (lset-difference = marked (list sel-pos)))
          (set! (*window-marked window) (cons sel-pos marked))))))

  (define-method (widget-clear-marked (window <window>))
    (set! (*window-marked window) '()))

  (define-method (widget-move (window <window>) n relative?)
    (list-box-move window n relative?))

  (define-method (widget-move-top (window <window>))
    (list-box-select window 0))

  (define-method (widget-move-bottom (window <window>))
    (list-box-select window (- (list-box-length window) 1)))

  ;; search {{{
  (define-method (widget-search (window <window>) query backward?)
    (unless (list-box-empty? window)
      (let ((i (if backward?
                 (window-search-backward window query)
                 (window-search-forward window query))))
        (when i (list-box-select window i)))))

  (define (window-search-forward window query)
    (define (match-fun row) (widget-match row query))
    (or (window-index window match-fun (+ 1 (list-box-sel-pos window)))
        (window-index window match-fun 0 (+ 1 (list-box-sel-pos window)))))

  (define (window-search-backward window query)
    (define (match-fun row) (widget-match row query))
    (or (window-index-right window match-fun 0 (list-box-sel-pos window))
        (window-index-right window match-fun (list-box-sel-pos window))))

  (define (window-index window pred? #!optional (start 0) (end (list-box-length window)))
    (let ((data (list-box-data window)))
      (let loop ((i start))
        (cond
          ((>= i end) #f)
          ((pred? (vector-ref data i)) i)
          (else (loop (+ i 1)))))))

  (define (window-index-right window pred? #!optional (start 0) (end (list-box-length window)))
    (let ((data (list-box-data window)))
      (let loop ((i (- end 1)))
        (cond
          ((< i start) #f)
          ((pred? (vector-ref data i)) i)
          (else (loop (- i 1)))))))
  ;; search }}}

  (define-method (handle-input (window <window>) input event)
    ;; Get the index of an item from a row number
    (define (get-index row)
      (let loop ((layout (container-layout/cached window))
                 (i 0))
        (if (null? layout)
          #f ;(- (list-box-length window) 1)
          (let ((child-y (third (car layout)))
                (child-rows (fifth (car layout))))
            (if (and (>= row child-y)
                     (< row (+ child-y child-rows)))
              (+ (list-box-top-pos window) i)
              (loop (cdr layout) (+ i 1)))))))
    (when (eqv? input KEY_MOUSE)
      (let ((index (get-index (- (mouse-event-y event)
                                 (widget-y window)))))
        (when index
          (mouse-case event
            ((BUTTON1_CLICKED BUTTON1_DOUBLE_CLICKED)
              (list-box-select window index))))))
    (call-next-method))

  (define (win-cursed-fun #!optional current?)
    (lambda (w i)
      (let ((current  (and current? (current? (list-box-ref w i))))
            (selected (= i (list-box-sel-pos w)))
            (marked   (memq i (window-marked w))))
        (cond ((and current selected) CURSED-WIN-CUR-SEL)
              (current                CURSED-WIN-CUR)
              (selected               CURSED-WIN-SEL)
              (marked                 CURSED-WIN-MARKED)
              (else                   CURSED-WIN)))))

  ;; <window> }}}
  ;; <window-row> {{{

  (define-class <window-row> (<widget>)
    ((data   reader:   window-row-data)
     (type   reader:   window-row-type)
     (format reader:   window-row-format)
     (cached initform: #f
             accessor: window-row-cached)))

  (define (make-window-row data type format)
    (make <window-row> 'data data 'type type 'format format))

  (define-method (widget-data (w <window-row>))
    (window-row-data w))

  (define-method (widget-invalidate (w <window-row>))
    (set! (window-row-cached w) #f))

  (define-method (window-row-text (widget <window-row>))
    (unless (window-row-cached widget)
      (let* ((fmt-slot (window-row-format widget))
             (row-fmt  (if (symbol? fmt-slot)
                         (get-option fmt-slot)
                         fmt-slot)))
        (set! (window-row-cached widget)
          (scmus-format row-fmt
                        (widget-cols widget)
                        (window-row-data widget)))))
    (window-row-cached widget))

  (define-method (print-widget! (w <window-row>) x y cols rows)
    (format-addstr! (window-row-text w) x y))

  (define-method (widget-match (row <window-row>) query)
    (let ((data (window-row-data row)))
      (case (window-row-type row)
        ((directory playlist artist album)
          (string-contains-ci (cdar data) query))
        ((file)
          (track-match data query))
        ((metadata)
          (or (string-contains-ci (format "~a" (cdar data)) query)
              (string-contains-ci (format "~a" (cdadr data)) query)))
        (else #f))))

  (define-method (widget-size (row <window-row>) cols rows)
    (values cols (min 1 rows)))

  ;; <window-row> }}}
  ;; <window-separator> {{{

  (define-class <window-separator> (<separator>)
    ((text   initform: ""
             accessor: window-separator-text)
     (indent initform: 0
             accessor: window-separator-indent)))

  (define-method (initialize-instance after: (w <window-separator>))
    (set! (widget-can-focus? w) #f))

  (define-method (print-widget! (widget <window-separator>) x y cols rows)
    (let ((text (string-append (make-string (window-separator-indent widget)
                                            (separator-char widget))
                               (window-separator-text widget))))
      (print-line! text x y cols (separator-char widget))))

  (define-method (widget-size (w <window-separator>) cols rows)
    (values cols (min 1 rows)))

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
