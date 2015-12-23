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

(define-class <container> (<widget>)
  ((children accessor: container-children)))

(define-class <split-pane> (<container>)
  ((left-size reader: split-pane-left-size)))

;; Ensure that left-size is between 0 and 1
(define-method ((setter split-pane-left-size) (pane <split-pane>) size)
  (cond
    ((and (>= size 0)
          (<= size 1))
      (set! (slot-value pane 'left-size) size))
    (else
      #f)))

;; Ensure that a split pane is always given 2 children
(define-method ((setter container-children) (pane <split-pane>) children)
  (cond
    ((= (length children) 2)
      (call-next-method))
    (else
      #f)))

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
;; Windows have @activate and @deactivate slots which store functions to be
;; called on particular events.  They don't belong here (TODO: move these to
;; view.scm).
;;

 (define-class <window> (<widget>)
  ((data       initform: '()
               reader:   *window-data
               writer:   *window-data-set!)
   (data-thunk initform: *window-data
               reader:   window-data-thunk)
   (data-len   initform: 0
               reader:   window-data-len
               writer:   *window-data-len-set!)
   (top-pos    initform: 0
               reader:   window-top-pos
               writer:   window-top-pos-set!)
   (sel-pos    initform: 0
               reader:   window-sel-pos
               writer:   window-sel-pos-set!)
   (marked     initform: '()
               reader:   *window-marked
               writer:   window-marked-set!)
   (nr-lines   initform: 0
               reader:   window-nr-lines
               writer:   *window-nr-lines-set!)
   (match-pos  initform: 0
               reader:   window-match-pos
               writer:   window-match-pos-set!)
   (changed    initform: void
               reader:   *window-changed!
               writer:   window-changed-set!)
   (activate   initform: void
               reader:   *window-activate!
               writer:   window-activate-set!)
   (deactivate initform: void
               reader:   *window-deactivate!
               writer:   window-deactivate-set!)
   (match      initform: (lambda (e q) #f)
               reader:   window-match
               writer:   window-match-set!)
   (query      initform: ""
               reader:   window-query
               writer:   window-query-set!)))

(define-method (initialize-instance (window <window>))
  (call-next-method)
  (window-data-len-update! window))

(define (window? obj)
  (subclass? (class-of obj) <window>))

(define (make-window . args)
  (apply make <window> args))

(: window-data (window -> list))
(define (window-data window)
  ((window-data-thunk window) window))

;; Whenever the length of the window data changes, we need to make sure that
;; the values of top-pos and sel-pos still make sense.
(: window-data-len-set! (window fixnum -> undefined))
(define (window-data-len-set! window len)
  (*window-data-len-set! window len)
  (when (>= (window-sel-pos window) len)
    (window-sel-pos-set! window (max 0 (- len 1))))
  (when (>= (window-top-pos window) len)
    (window-top-pos-set! window (max 0 (- len 1)))))

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
  ((*window-changed! window) window))

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
      (window-marked-set! window (cons sel-pos marked)))))

(: window-unmark! (window -> undefined))
(define (window-unmark! window)
  (let ((sel-pos (window-sel-pos window))
        (marked (*window-marked window)))
    (if (and (positive? (window-data-len window)) (member sel-pos marked))
      (window-marked-set! window (remove (lambda (x) (= x sel-pos)) marked)))))

;; toggles the 'marked' status of the selected row
(: window-toggle-mark! (window -> undefined))
(define (window-toggle-mark! window)
  (let ((sel-pos (window-sel-pos window))
        (marked (*window-marked window)))
    (when (positive? (window-data-len window))
      (if (member sel-pos marked)
        (window-marked-set! window (remove (lambda (x) (= x sel-pos)) marked))
        (window-marked-set! window (cons sel-pos marked))))))

(: window-clear-marked! (window -> undefined))
(define (window-clear-marked! window)
  (window-marked-set! window '())
  (window-changed! window))

(: window-activate! (window -> undefined))
(define (window-activate! window)
  (when (positive? (window-data-len window))
    ((*window-activate! window) window)))

(: window-deactivate! (window -> undefined))
(define (window-deactivate! window)
  (when (positive? (window-data-len window))
    ((*window-deactivate! window) window)))

(: window-data-len-update! (window -> undefined))
(define (window-data-len-update! window)
  (window-data-len-set! window (length (window-data window))))

(: window-move-down! (window fixnum -> undefined))
(define (window-move-down! window n)
  (let* ((top-pos (window-top-pos window))
         (sel-pos (window-sel-pos window))
         (data-len (window-data-len window))
         (nr-lines (window-nr-lines window))
         (can-move (max 0 (min n (- data-len sel-pos 1))))
         (scroll (max 0 (+ 1 (- (+ sel-pos can-move)
                                (+ top-pos nr-lines))))))
    (window-top-pos-set! window (+ top-pos scroll))
    (window-sel-pos-set! window (+ sel-pos can-move))
    (window-changed! window)))

(: window-move-up! (window fixnum -> undefined))
(define (window-move-up! window n)
  (let* ((top-pos (window-top-pos window))
         (sel-pos (window-sel-pos window))
         (can-move (min n sel-pos))
         (scroll (max 0 (- can-move
                           (- sel-pos top-pos)))))
    (window-top-pos-set! window (- top-pos scroll))
    (window-sel-pos-set! window (- sel-pos can-move))
    (window-changed! window)))

(: window-nr-lines-set! (window fixnum -> undefined))
(define (window-nr-lines-set! window nr-lines)
  (let ((top-pos (window-top-pos window))
        (sel-pos (window-sel-pos window)))
    (when (<= nr-lines (- sel-pos top-pos))
      (window-top-pos-set! window
                           (+ top-pos
                              (- (window-nr-lines window)
                                 nr-lines)))))
  (*window-nr-lines-set! window nr-lines))

(: window-select! (window fixnum -> undefined))
(define (window-select! window i)
  (let* ((sel-pos (window-sel-pos window))
         (diff (- sel-pos i)))
    (cond
      ((> diff 0) (window-move-up! window diff))
      ((< diff 0) (window-move-down! window (abs diff))))))

(: window-search-init! (window string -> undefined))
(define (window-search-init! window query)
  (window-query-set! window query)
  (window-match-pos-set! window (window-sel-pos window)))

(: window-next-match! (window -> (or fixnum boolean)))
(define (window-next-match! window)
  (let* ((query (window-query window))
         (last-pos (window-match-pos window))
         (last-match (list-tail (window-data window) last-pos)))
    (let loop ((pos (+ last-pos 1))
               (rest (if (null? last-match) '() (cdr last-match))))
      (cond
        ((and (= pos last-pos) (null? rest)) #f)
        ((= pos last-pos)
          (if ((window-match window) (car rest) query)
            pos
            #f))
        ((null? rest) (loop 0 (window-data window)))
        (((window-match window) (car rest) query)
          (window-match-pos-set! window pos)
          pos)
        (else (loop (+ pos 1) (cdr rest)))))))

(: window-prev-match! (window -> (or fixnum boolean)))
(define (window-prev-match! window)
  (let ((orig-pos (window-match-pos window)))
    (let loop ((last-pos orig-pos))
      (let ((rv (window-next-match! window)))
        (cond
          ((not rv) #f)
          ((= rv orig-pos)
            (window-match-pos-set! window last-pos)
            last-pos)
          (else (loop rv)))))))
