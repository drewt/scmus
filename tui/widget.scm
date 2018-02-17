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

(module scmus.tui.widget *
  (import coops)
  (import scmus.base)

  (define-class <widget> ()
    ((parent  initform: #f
              accessor: widget-parent)
     (damaged initform: #t
              accessor: widget-damaged)))

  (define-method (widget-damaged! (widget <widget>))
    ; FIXME: widgets need to store their own geometry in order to do partial
    ;        redraws.  For now, we just mark the root widget as damaged so that
    ;        the whole hierarchy is redrawn.
    ;
    ;        OR a better idea: have a GET-GEOMETRY method which returns:
    ;            * the terminal geometry for root widgets
    ;            * the result of calling GET-CHILD-GEOMETRY on the parent widget
    ;              for non-root widgets
    ;       All container widgets must implement the GET-CHILD-GEOMETRY method:
    ;           (GET-CHILD-GEOMETRY <PARENT> <CHILD>)
    ;       "Geometry" is a pair of pairs: ((X . Y) . (WIDTH . HEIGHT))
    ;       Where (X . Y) is the top-leftmost coordinate of the widget, and
    ;       (WIDTH . HEIGHT) are as you would expect.
    (set! (widget-damaged (widget-root widget)) #t))

  (define-method (widget-geometry-set! (widget <widget>) cols rows)
    (void))

  (define-method (widget-first (widget <widget>))
    widget)

  (define-method (widget-last (widget <widget>))
    widget)

  (define-method (widget-root (widget <widget>))
    (if (widget-parent widget)
      (widget-root (widget-parent widget))
      widget))

  (define-method (widget-focus (widget <widget>))
    widget)

  ;;
  ;; Container
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <container> (<widget>)
    ((children accessor: container-children)))

  (define-generic (compute-layout container cols rows))
  
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

  (define-method (widget-focus (container <container>))
    (if (null? (container-children container))
      #f
      (widget-focus (car (container-children container)))))

  ;; Generic <container> printing method.  Subclasses can override this to add borders, etc.
  (define-method (print-widget! (container <container>) x y cols rows)
    (define (adjust-positions layout)
      (append (list (car layout)
                    (+ x (cadr layout))
                    (+ y (caddr layout)))
              (cdddr layout)))
    (for-each (lambda (child)
                (apply print-widget! (adjust-positions child)))
              (compute-layout container cols rows)))

  )
