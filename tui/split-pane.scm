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

(module scmus.tui.split-pane (<split-pane>
                              split-pane-left-child
                              split-pane-right-child
                              split-pane-left-size
                              split-pane-separator-char
                              make-split-pane)
  (import coops
          scmus.base
          scmus.tui.misc
          scmus.tui.widget)

  (define-class <split-pane> (<container>)
    ((left-child     accessor: split-pane-left-child)
     (right-child    accessor: split-pane-right-child)
     (left-size      initform: 0.5
                     reader:   split-pane-left-size)
     (focus-left     initform: #t)
     (separator-char initform: #\space
                     reader: split-pane-separator-char)))

  (define (make-split-pane left-child right-child . args)
    (apply make <split-pane> 'left-child left-child 'right-child right-child args))

  ;; Ensure that left-size is between 0 and 1
  (define-method ((setter split-pane-left-size) (pane <split-pane>) size)
    (cond
      ((and (>= size 0)
            (<= size 1))
        (set! (slot-value pane 'left-size) size))
      (else
        #f)))

  (define-method (container-children (pane <split-pane>))
    (list (split-pane-left-child pane)
          (split-pane-right-child pane)))

  (define-method (widget-focus (pane <split-pane>))
    (widget-focus
      (if (slot-value pane 'focus-left)
        (split-pane-left-child pane)
        (split-pane-right-child pane))))

  (define-method (widget-size (pane <split-pane>) available-cols available-rows)
    (let-values (((l-cols l-rows) (widget-size (split-pane-left-child pane)
                                               available-cols
                                               available-rows))
                 ((r-cols r-rows) (widget-size (split-pane-right-child pane)
                                               available-cols
                                               available-rows)))
      (values (min available-cols l-cols r-cols)
              (min available-rows r-rows l-rows))))

  (define-method (compute-layout (pane <split-pane>) cols rows)
    ; FIXME: don't create new separator for every draw
    (let* ((separator (make <separator> 'char (split-pane-separator-char pane)))
           (left-cols (inexact->exact (floor (* (split-pane-left-size pane) cols))))
           (right-cols (- cols left-cols 1)))
      ;           WIDGET                        X               Y COLS       ROWS CURSED
      (list (list (split-pane-left-child pane)  0               0 left-cols  rows #f)
            (list separator                     left-cols       0 1          rows #f)
            (list (split-pane-right-child pane) (+ left-cols 1) 0 right-cols rows #f)))))
