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

(require-extension srfi-1)

(declare (unit window)
         (uses ncurses))

;; A "window" is a view of a list
(define-record-type window
  (*make-window data data-thunk data-len
               top-pos sel-pos marked nr-lines match-pos
               changed activate deactivate
               match query)
  window?
  ;; private data member for internal use
  (data *window-data *window-data-set!)
  ;; thunk to retrieve the list
  (data-thunk window-data-thunk)
  ;; length of the list
  (data-len window-data-len *window-data-len-set!)
  ;; position of the first visible row
  (top-pos window-top-pos window-top-pos-set!)
  ;; position of the selected row
  (sel-pos window-sel-pos window-sel-pos-set!)
  ;; list of 'marked' rows
  (marked *window-marked window-marked-set!)
  ;; number of visible rows
  (nr-lines window-nr-lines *window-nr-lines-set!)
  ;; position of the last match
  (match-pos window-match-pos window-match-pos-set!)
  ;; called when visible part of list has changed
  (changed *window-changed! window-changed-set!)
  ;; function to call when the user "activates" the window
  (activate *window-activate! window-activate-set!)
  ;; function to call when the user "deactivates" the window
  (deactivate *window-deactivate! window-deactivate-set!)
  ;; function to match a search query against a row
  (match window-match window-match-set!)
  ;; active search query
  (query window-query window-query-set!))

(define (make-window #!key
                     (data '())
                     (get-data *window-data)
                     (changed void)
                     (activate void)
                     (deactivate void)
                     (match (lambda (e q) #f)))
  (let ((window (*make-window data
                              get-data
                              0
                              0
                              0
                              '()
                              (- (LINES) 4)
                              0
                              changed
                              activate
                              deactivate
                              match
                              "")))
    (window-data-len-update! window)
    window))

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
  (window-marked-set! window '()))

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
