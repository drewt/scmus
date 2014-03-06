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

(declare (unit window)
         (uses ui-curses))

;; A "window" is a view of a list
(define-record-type window
  (make-window data data-thunk data-len
               top-pos sel-pos nr-lines
               changed activate deactivate)
  window?
  ;; private data member for internal use
  (data *window-data *window-data-set!)
  ;; thunk to retrieve the list
  (data-thunk window-data-thunk)
  ;; length of the list
  (data-len window-data-len window-data-len-set!)
  ;; position of the first visible row
  (top-pos window-top-pos window-top-pos-set!)
  ;; position of the selected row
  (sel-pos window-sel-pos window-sel-pos-set!)
  ;; number of visible rows
  (nr-lines window-nr-lines *window-nr-lines-set!)
  ;; called when visible part of list has changed
  (changed *window-changed! window-changed-set!)
  ;; function to call when the user "activates" the window
  (activate *window-activate! window-activate-set!)
  ;; function to call when the user "deactivates" the window
  (deactivate *window-deactivate! window-deactivate-set!))

(define (window-data window)
  (assert (window? window))
  ((window-data-thunk window) window))

(define (window-top window)
  (assert (window? window))
  (assert (>= (length (window-data window)) (window-top-pos window)))
  (list-tail (window-data window) (window-top-pos window)))

(define (window-selected window)
  (assert (window? window))
  (assert (> (length (window-data window)) (window-sel-pos window)))
  (list-ref (window-data window) (window-sel-pos window)))

(define (window-changed! window)
  (assert (window? window))
  ((*window-changed! window) window))

(define (window-activate! window)
  (assert (window? window))
  ((*window-activate! window) window))

(define (window-deactivate! window)
  (assert (window? window))
  ((*window-deactivate! window) window))

(define (window-data-len-update! window)
  (assert (window? window))
  (let ((top-pos (window-top-pos window))
        (sel-pos (window-sel-pos window))
        (nr-lines (window-nr-lines window))
        (new-len (length (window-data window))))
    (window-data-len-set! window new-len)
    (if (>= top-pos new-len)
      (window-top-pos-set! window (max 0 (- new-len 1))))
    (if (>= sel-pos new-len)
      (window-sel-pos-set! window (max 0 (- new-len 1))))))

(define (window-move-down! window n)
  (assert (window? window))
  (let* ((top-pos (window-top-pos window))
         (sel-pos (window-sel-pos window))
         (data-len (window-data-len window))
         (nr-lines (window-nr-lines window))
         (can-move (min n (- data-len sel-pos 1)))
         (scroll (max 0 (+ 1 (- (+ sel-pos can-move)
                                (+ top-pos nr-lines))))))
    (window-top-pos-set! window (+ top-pos scroll))
    (window-sel-pos-set! window (+ sel-pos can-move))
    (window-changed! window)))

(define (window-move-up! window n)
  (assert (window? window))
  (let* ((top-pos (window-top-pos window))
         (sel-pos (window-sel-pos window))
         (can-move (min n sel-pos))
         (scroll (max 0 (- can-move
                           (- sel-pos top-pos)))))
    (window-top-pos-set! window (- top-pos scroll))
    (window-sel-pos-set! window (- sel-pos can-move))
    (window-changed! window)))

(define (window-nr-lines-set! window nr-lines)
  (assert (window? window))
  (assert (>= nr-lines 0))
  (let ((top-pos (window-top-pos window))
        (sel-pos (window-sel-pos window)))
    (when (<= nr-lines (- sel-pos top-pos))
      (window-top-pos-set! window
                           (+ top-pos
                              (- (window-nr-lines window)
                                 nr-lines)))))
  (*window-nr-lines-set! window nr-lines))
