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

(require-extension srfi-1 coops)

(declare (unit window))

(define-class <widget> ()
  ((parent initform: #f
           accessor: widget-parent)))

(define-method (widget-geometry-set! (widget <widget>) cols rows)
  (void))

(define-method (widget-first (widget <widget>))
  widget)

(define-method (widget-last (widget <widget>))
  widget)

(define-method (widget-next (widget <widget>) (prev <widget>))
  (if (widget-parent widget)
    (widget-next (widget-parent widget) widget)
    #f))

(define-method (widget-prev (widget <widget>) (next <widget>))
  (if (widget-parent widget)
    (widget-prev (widget-parent widget) widget)
    #f))

(define-class <separator> (<widget>)
  ((char initform: #\space
         accessor: separator-char)))

(define-class <container> (<widget>)
  ((children accessor: container-children)))

(define-method (container-prepend-child! (container <container>) (child <widget>))
  (set! (widget-parent child) container)
  (set! (container-children container)
    (cons child (container-children container))))

(define-method (container-append-child! (container <container>) (child <widget>))
  (set! (widget-parent child) container)
  (set! (container-children container)
    (append! (container-children container) (list child))))

(define-method (widget-first (container <container>))
  (widget-first (car (container-children container))))

(define-method (widget-last (container <container>))
  (widget-last (car (reverse (container-children container)))))

(: *container-next-child (list (struct widget) -> (or boolean (struct widget))))
(define (*container-next-child children child)
  (let ((rest (member child children)))
    (cond
      ((not rest)         #f) ; FIXME: throw exception
      ((null? (cdr rest)) (car children))
      (else               (cadr rest)))))

(define-method (widget-next (container <container>) (child <widget>))
  (let ((next (*container-next-child (container-children container) child)))
    (if next
      (widget-first next)
      #f)))

(define-method (widget-prev (container <container>) (child <widget>))
  (let ((prev (*container-next-child (reverse (container-children container)) child)))
    (if prev
      (widget-last prev)
      prev)))

;;
;; Split Pane
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <split-pane> (<container>)
  ((left-size      initform: 0.5
                   reader: split-pane-left-size)
   (separator-char initform: #\space
                   reader: split-pane-separator-char)))

(define (make-split-pane left-child right-child . args)
  (let* ((children (list left-child right-child))
         (pane (apply make <split-pane> 'children children args)))
    (set! (widget-parent left-child) pane)
    (set! (widget-parent right-child) pane)
    pane))

;; Ensure that left-size is between 0 and 1
(define-method ((setter split-pane-left-size) (pane <split-pane>) size)
  (cond
    ((and (>= size 0)
          (<= size 1))
      (set! (slot-value pane 'left-size) size))
    (else
      #f)))

(define-method (split-pane-left-child (pane <split-pane>))
  (car (container-children pane)))

(define-method (split-pane-right-child (pane <split-pane>))
  (cadr (container-children pane)))

;; Ensure that a split pane is always given 2 children
(define-method ((setter container-children) (pane <split-pane>) children)
  (cond
    ((= (length children) 2)
      (call-next-method))
    (else
      #f)))

(define-method (compute-layout (pane <split-pane>) cols rows)
  (let* ((separator (make <separator> 'char (split-pane-separator-char pane)))
         (left-cols (inexact->exact (floor (* (split-pane-left-size pane) cols))))
         (right-cols (- cols left-cols 1)))
    ;           WIDGET                        X               Y COLS       ROWS
    (list (list (split-pane-left-child pane)  0               0 left-cols  rows)
          (list separator                     left-cols       0 1          rows)
          (list (split-pane-right-child pane) (+ left-cols 1) 0 right-cols rows))))

(define-method (widget-geometry-set! (pane <split-pane>) cols rows)
  (let loop ((children (compute-layout pane cols rows)))
    (unless (null? children)
      (widget-geometry-set! (first (car children))
                            (fourth (car children))
                            (fifth  (car children)))
      (loop (cdr children)))))

;;
;; Window
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
;; By default, the list is given by the @data slot of the window object.
;;
;; If the @data-thunk slot is set to something other than the default, then
;; the @data slot is ignored and the return value of @data-thunk is used
;; instead.  This can be used to track a list which is stored outside of the
;; window, for example.  It is not advisable to do any expensive computations
;; in @data-thunk, as it is called often.
;;
;; A window can be searched.  This works by calling the function in the @match
;; slot on each row of the window; if @match returns true, then the row is
;; considered to be a match for the query.
;;
;; When the visible part of a window changes, the function in the @changed
;; slot is called.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define-class <window> (<widget>)
  ((data       initform: '()
               accessor: *window-data)
   (data-thunk initform: *window-data
               accessor: window-data-thunk)
   (data-len   initform: 0
               ; complex writer below
               reader:   window-data-len)
   (top-pos    initform: 0
               accessor: window-top-pos)
   (sel-pos    initform: 0
               accessor: window-sel-pos)
   (marked     initform: '()
               accessor: *window-marked)
   (nr-lines   initform: 0
               ; complex writer below
               reader:   window-nr-lines)
   (query      initform: ""
               accessor: window-query) 
   (h-border   initform: 1
               accessor: window-h-border)
   ;; Because COOPS does not play well across multiple compilation units, we
   ;; use our own OOP implementation for customizing windows.  Ideally, the
   ;; functions stored in the slots below would be defined as methods, but
   ;; then they couldn't be specialized in another compilation unit.
   (changed    initform: void
               accessor: window-changed)
   (activate   initform: void
               accessor: window-activate)
   (deactivate initform: void
               accessor: window-deactivate)
   (match      initform: (lambda (e q) #f)
               accessor: window-match)
   (add        initform: void
               accessor: window-add)
   (remove     initform: void
               accessor: window-remove)
   (clear      initform: void
               accessor: window-clear)
   (edit       initform: void
               accessor: window-edit)
   (move       initform: void
               accessor: window-move)
   (cursed     initform: (win-cursed-fn (lambda (x) #f))
               reader:   *window-cursed)
   (print-line initform: (lambda (window row cols) (format "~a" row))
               reader:   *window-print-line)))

(define-method (initialize-instance (window <window>))
  (call-next-method)
  (window-data-len-update! window))

(define-syntax define-subclass-predicate
  (syntax-rules ()
    ((define-subclass-predicate predname classname)
      (define (predname obj)
        (subclass? (class-of obj) classname)))))

(define-subclass-predicate widget? <widget>)
(define-subclass-predicate container? <container>)
(define-subclass-predicate window? <window>)

(define-method (widget-geometry-set! (window <window>) cols rows)
  (set! (window-nr-lines window) rows))

(define (make-window . args)
  (apply make <window> args))

(define-class <window-stack> (<window>)
  ((stack initform: '()
          accessor: window-stack-stack)))

(define-method (window-stack-push! (window <window-stack>) data data-thunk)
  (set! (window-stack-stack window)
    (cons `((data       . ,(*window-data window))
            (data-thunk . ,(window-data-thunk window))
            (data-len   . ,(window-data-len window))
            (top-pos    . ,(window-top-pos window))
            (sel-pos    . ,(window-sel-pos window))
            (marked     . ,(*window-marked window)))
          (window-stack-stack window)))
  (set! (*window-data window) data)
  (set! (window-data-thunk window) data-thunk)
  (set! (window-top-pos window) 0)
  (set! (window-sel-pos window) 0)
  (set! (*window-marked window) '())
  (window-data-len-update! window))

(define-method (window-stack-pop! (window <window-stack>))
  (let loop ((members (car (window-stack-stack window))))
    (unless (null? members)
      (let ((name  (caar members))
            (value (cdar members)))
        (case name
          ((data)       (set! (*window-data window) value))
          ((data-thunk) (set! (window-data-thunk window) value))
          ((data-len)   (set! (window-data-len window) value))
          ((top-pos)    (set! (window-top-pos window) value))
          ((sel-pos)    (set! (window-sel-pos window) value))
          ((marked)     (set! (*window-marked window) value))))
      (loop (cdr members))))
  (set! (window-stack-stack window)
    (cdr (window-stack-stack window))))

(define-method (window-stack-peek (window <window-stack>))
  (let ((stack (window-stack-stack window)))
    (if (null? stack)
      #f
      (car stack))))

(define (make-stack-window . args)
  (apply make <window-stack> args))

(: window-data (window -> list))
(define (window-data window)
  ((window-data-thunk window) window))

;; Whenever the length of the window data changes, we need to make sure that
;; the values of top-pos and sel-pos still make sense.
(define-method ((setter window-data-len) (window <window>) len)
  (set! (slot-value window 'data-len) len)
  (when (>= (window-sel-pos window) len)
    (set! (window-sel-pos window) (max 0 (- len 1))))
  (when (>= (window-top-pos window) len)
    (set! (window-top-pos window) (max 0 (- len 1)))))

(: window-sel-offset (window -> fixnum))
(define (window-sel-offset window)
  (- (window-sel-pos window)
     (window-top-pos window)))

(: window-top (window -> list))
(define (window-top window)
  (assert (>= (length (window-data window)) (window-top-pos window)))
  (list-tail (window-data window) (window-top-pos window)))

(: window-selected (window -> *))
(define (window-selected window)
  (assert (> (length (window-data window)) (window-sel-pos window))
          "window-selected")
  (list-ref (window-data window) (window-sel-pos window)))

(: window-all-selected (window -> list))
(define (window-all-selected window)
  (define (select-from indices lst)
    (cdr (fold (lambda (x acc)
                 (if (member (car acc) indices)
                   (cons (+ (car acc) 1)
                         (cons x (cdr acc)))
                   (cons (+ (car acc) 1)
                         (cdr acc))))
               '(0 . ())
               lst)))
  (let* ((sel-pos (window-sel-pos window))
         (marked (window-marked window)))
    (reverse (select-from marked (window-data window)))))

(: window-changed! (window -> undefined))
(define (window-changed! window)
  ((window-changed window) window))

;; XXX: selected row counts as marked
(: window-marked (window -> list))
(define (window-marked window)
  (let ((sel-pos (window-sel-pos window))
        (marked (*window-marked window)))
    (if (member sel-pos marked)
      marked
      (cons sel-pos marked))))

(: window-mark! (window -> undefined))
(define (window-mark! window)
  (let ((sel-pos (window-sel-pos window))
        (marked (*window-marked window)))
    (unless (or (<= (window-data-len window) 0) (member sel-pos marked))
      (set! (*window-marked window) (cons sel-pos marked)))))

(: window-unmark! (window -> undefined))
(define (window-unmark! window)
  (let ((sel-pos (window-sel-pos window))
        (marked (*window-marked window)))
    (if (and (positive? (window-data-len window)) (member sel-pos marked))
      (set! (*window-marked window) (remove (lambda (x) (= x sel-pos)) marked)))))

;; toggles the 'marked' status of the selected row
(: window-toggle-mark! (window -> undefined))
(define (window-toggle-mark! window)
  (let ((sel-pos (window-sel-pos window))
        (marked (*window-marked window)))
    (when (positive? (window-data-len window))
      (if (member sel-pos marked)
        (set! (*window-marked window) (remove (lambda (x) (= x sel-pos)) marked))
        (set! (*window-marked window) (cons sel-pos marked))))))

(: window-clear-marked! (window -> undefined))
(define (window-clear-marked! window)
  (set! (*window-marked window) '())
  (window-changed! window))

(: window-activate! (window -> undefined))
(define (window-activate! window)
  (when (positive? (window-data-len window))
    ((window-activate window) window)))

(: window-deactivate! (window -> undefined))
(define (window-deactivate! window)
  (when (positive? (window-data-len window))
    ((window-deactivate window) window)))

(: window-add! (window -> undefined))
(define (window-add! window)
  (when (positive? (window-data-len window))
    ((window-add window) window)))

(: window-remove! (window -> undefined))
(define (window-remove! window)
  (when (positive? (window-data-len window))
    ((window-remove window) window)))

(: window-clear! (window -> undefined))
(define (window-clear! window)
  (when (positive? (window-data-len window))
    ((window-clear window) window)))

(: window-edit! (window -> undefined))
(define (window-edit! window)
  (when (positive? (window-data-len window))
    ((window-edit window) window)))

(: window-move! (window -> undefined))
(define (window-move! window)
  (when (positive? (window-data-len window))
    ((window-move window) window)))

(: window-print-line (window * fixnum -> string))
(define (window-print-line window row nr-cols)
  (let* ((h-border (window-h-border window))
         (str ((*window-print-line window) window
                                           row
                                           (- nr-cols (* 2 h-border)))))
    (string-append (make-string h-border #\space)
                   str
                   (make-string h-border #\space))))

(: window-cursed (window * fixnum -> undefined))
(define (window-cursed window row line-nr)
  ((*window-cursed window) window row line-nr))

(: window-data-len-update! (window -> undefined))
(define (window-data-len-update! window)
  (set! (window-data-len window) (length (window-data window))))

(: window-move-down! (window fixnum -> undefined))
(define (window-move-down! window n)
  (let* ((top-pos  (window-top-pos window))
         (sel-pos  (window-sel-pos window))
         (data-len (window-data-len window))
         (nr-lines (window-nr-lines window))
         (can-move (max 0 (min n (- data-len sel-pos 1))))
         (scroll   (max 0 (+ 1 (- (+ sel-pos can-move)
                                  (+ top-pos nr-lines))))))
    (set! (window-top-pos window) (+ top-pos scroll))
    (set! (window-sel-pos window) (+ sel-pos can-move))
    (window-changed! window)))

(: window-move-up! (window fixnum -> undefined))
(define (window-move-up! window n)
  (let* ((top-pos (window-top-pos window))
         (sel-pos (window-sel-pos window))
         (can-move (min n sel-pos))
         (scroll (max 0 (- can-move
                           (- sel-pos top-pos)))))
    (set! (window-top-pos window) (- top-pos scroll))
    (set! (window-sel-pos window) (- sel-pos can-move))
    (window-changed! window)))

(define-method ((setter window-nr-lines) (window <window>) nr-lines)
  (let ((top-pos (window-top-pos window))
        (sel-pos (window-sel-pos window)))
    (when (<= nr-lines (- sel-pos top-pos))
      (set! (window-top-pos window)
            (+ top-pos
               (- (window-nr-lines window)
                  nr-lines)))))
  (set! (slot-value window 'nr-lines) nr-lines))

(: window-select! (window fixnum -> undefined))
(define (window-select! window i)
  (let* ((sel-pos (window-sel-pos window))
         (diff (- sel-pos i)))
    (cond
      ((> diff 0) (window-move-up! window diff))
      ((< diff 0) (window-move-down! window (abs diff))))))

(: window-search-init! (window string -> undefined))
(define (window-search-init! window query)
  (set! (window-query window) query))

(: *window-search (window string list -> (or fixnum boolean)))
(define (*window-search window query data)
  (let loop ((data data) (i 0))
    (cond
      ((null? data) #f)
      (((window-match window) (car data) query) i)
      (else (loop (cdr data) (+ 1 i))))))

(: window-next-match! (window -> (or fixnum boolean)))
(define (window-next-match! window)
  (let* ((query    (window-query window))
         (data     (window-data window))
         (next-pos (+ 1 (window-sel-pos window)))
         (next-len (- (window-data-len window) next-pos))
         (shifted  (append (drop data next-pos)
                           (take data next-pos)))
         (match    (*window-search window query shifted)))
    (cond
      ((not match)         #f)
      ((>= match next-len) (- match next-len))
      (else                (+ next-pos match)))))

(: window-prev-match! (window -> (or fixnum boolean)))
(define (window-prev-match! window)
  (let* ((query    (window-query window))
         (data     (window-data window))
         (prev-pos (- (window-sel-pos window) 1))
         (shifted  (append (reverse (take data (+ 1 prev-pos)))
                           (reverse (drop data (+ 1 prev-pos)))))
         (match    (*window-search window query shifted)))
    (cond
      ((not match)        #f)
      ((> match prev-pos) (- (window-data-len window)
                             (abs (- prev-pos match))))
      (else               (- prev-pos match)))))
