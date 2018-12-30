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

(module scmus.tui.list-box *
  (import (only extras pp))
  (import coops
          vector-lib
          drewt.ncurses
          scmus.base
          scmus.tui.display
          scmus.tui.input
          scmus.tui.widget)

  (define-class <list-box> (<container>)
    ((data       initform: #()
                 reader:   list-box-data)
     (sel-pos    initform: 0
                 reader:   list-box-sel-pos)
     (top-pos    initform: 0
                 accessor: list-box-top-pos)
     (nr-visible initform: 0
                 accessor: list-box-nr-visible)
     (cursed-fun initform: (lambda (w i) #f)
                 accessor: list-box-cursed-fun)))

  (define-method (initialize-instance (w <list-box>))
    (call-next-method)
    (cond
      ((vector? (list-box-data w)) (void))
      ((list? (list-box-data w))
        (set! (slot-value w 'data) (list->vector (list-box-data w))))
      (else (assert #f "initialize-instance" w))))

  (define-method ((setter list-box-data) (w <list-box>) data)
    ; convert data to vector
    (let* ((data (cond
                   ((vector? data) data)
                   ((list? data)   (list->vector data))
                   (else           (assert #f "(setter list-box-data)" w data))))
           (len  (vector-length data)))
      (set! (slot-value w 'data) data)
      ; set parent on children
      (vector-for-each (lambda (i child)
                         (set! (widget-parent child) w))
                       data)
      ; update pointers into data
      ; XXX: we use SLOT-VALUE to avoid calling setters
      ;      (setters assume data is unchanged)
      (when (>= (list-box-sel-pos w) len)
        (set! (slot-value w 'sel-pos) (max 0 (- len 1))))
      (when (>= (list-box-top-pos w) len)
        (set! (slot-value w 'top-pos) (max 0 (- len 1)0)))
      (widget-damaged! w)))

  (define (list-box-cursed w i)
    ((list-box-cursed-fun w) w i))

  ;; Changing the cursor location is rather complicated, unfortunately.
  ;; If the item at the new sel-pos doesn't accept focus, we try to find
  ;; the first item 'after' the given pos that does ('after' can mean
  ;; 'before' or 'after', depending on which direction the cursor is
  ;; moving).  If we can't find a focusable widget 'after' the given pos,
  ;; we try looking in the other direction (otherwise you couldn't move
  ;; to the topmost/bottommost item when it doesn't accept focus).
  ;;
  ;; With all that said and done, we also update the colors of the old
  ;; and new items and mark them damaged.
  (define-method ((setter list-box-sel-pos) (w <list-box>) new-pos)
    (let* ((old-pos (list-box-sel-pos w))
           (len     (list-box-length w)))
      (let loop ((this-pos new-pos)
                 (inc      (if (positive? (- new-pos old-pos)) 1 -1))
                 (retried? #f))
        (cond
          ; reached end of data; either reverse direction or give up
          ((or (< this-pos 0) (>= this-pos len))
            (unless retried?
              (loop (- new-pos inc) (* inc -1) #t)))
          ; widget is focusable, select it
          ((widget-can-focus? (list-box-ref w this-pos))
            (let ((old (list-box-ref w old-pos))
                  (new (list-box-ref w this-pos)))
              (set! (slot-value w 'sel-pos) this-pos)
              (set! (widget-cursed/cached old) (list-box-cursed w old-pos))
              (set! (widget-cursed/cached new) (list-box-cursed w this-pos))
              (widget-damaged! old)
              (widget-damaged! new)))
          ; continue looking in the same direction
          (else (loop (+ this-pos inc) inc retried?))))))

  (define-method ((setter list-box-top-pos) after: (w <list-box>) pos)
    (widget-damaged! w))

  (define-method ((setter list-box-cursed-fun) after: (w <list-box>) fun)
    (widget-damaged! w))

  ;; Helper function to get negotiated height of a child widget.
  (define (child-height w i cols rows)
    (let-values (((cols rows) (widget-size (vector-ref (list-box-data w) i) cols rows)))
      rows))

  (define (list-box-select/first w pos)
    (set! (list-box-top-pos w) pos)
    (set! (list-box-sel-pos w) pos))

  (define (list-box-select/last w pos)
    (define (new-top-pos)
      (let ((rows    (widget-rows w))
            (cols    (widget-cols w))
            (data    (list-box-data w)))
        (let loop ((i (- pos 1))
                   (h (child-height w pos cols rows)))
          (if (or (< i 0)
                  (>= h rows))
            (+ i 1)
            (let ((new-h (+ h (child-height w i cols rows))))
              (if (> new-h rows)
                (+ i 1)
                (loop (- i 1) new-h)))))))
    (set! (list-box-top-pos w) (new-top-pos))
    (set! (list-box-sel-pos w) pos))

  (define (list-box-visible? w index)
    (let ((cols (widget-cols w))
          (rows (widget-rows w))
          (top  (list-box-top-pos w))
          (data (list-box-data w))
          (nr-v (list-box-nr-visible w)))
      (cond
        ((< index top) #f)
        ((< index (+ top nr-v)) #t)
        (else #f))))

  (define-method ((setter widget-rows) after: (w <list-box>) rows)
    (unless (or (list-box-empty? w)
                (list-box-visible? w (list-box-sel-pos w)))
      (list-box-select/last w (list-box-sel-pos w))))

  (define-method (widget-focus (w <list-box>))
    (if (list-box-empty? w)
      w
      (widget-focus (list-box-selected w))))

  (define-method (container-children (w <list-box>))
    (vector->list (list-box-data w)))

  (define-method (compute-layout (w <list-box>) cols rows)
    (let ((data (list-box-data w)))
      (let loop ((i (list-box-top-pos w))
                 (y 0)
                 (result '()))
        (if (or (>= y rows)
                (>= i (vector-length data)))
          (begin
            ; XXX: save number of visible children
            (set! (list-box-nr-visible w) (length result))
            (reverse result))
          (let ((rows (nth-value 1 (widget-size (vector-ref data i) cols (- rows y)))))
            (loop (+ i 1)
                  (+ y rows)
                  (cons (list (vector-ref data i) 0 y cols rows (list-box-cursed w i))
                        result)))))))

  (define-method (list-box-length (w <list-box>))
    (vector-length (list-box-data w)))

  (define-method (list-box-empty? (w <list-box>))
    (zero? (vector-length (list-box-data w))))

  (define-method (list-box-ref (w <list-box>) i)
    (vector-ref (list-box-data w) i))

  (define-method (list-box-selected (w <list-box>))
    (vector-ref (list-box-data w) (list-box-sel-pos w)))

  ;; Move the cursor up/down N items.  Positive values of N move down, negative up.
  (define-method (list-box-move (w <list-box>) n relative?)
    (if (not relative?)
      (list-box-select w (max 0 (min (- (list-box-length w) 1)
                                     (+ (list-box-sel-pos w) n))))
      (let ((rows (integer-scale (widget-rows w) n)))
        ; FIXME: this makes the assumption that each item is 1 row high
        (list-box-move w rows #f))))

  ;; Move the cursor to a given index.
  (define-method (list-box-select (w <list-box>) i)
    (cond
      ((list-box-empty? w)        (void))
      ((list-box-visible? w i)    (set! (list-box-sel-pos w) i))
      ((< i (list-box-top-pos w)) (list-box-select/first w i))
      (else                       (list-box-select/last w i)))))
