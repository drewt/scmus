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
  (import coops
          scmus.base
          scmus.tui.display)

  (define *damaged-widgets* '())
  (define (damaged-widgets) *damaged-widgets*)
  (define (clear-damaged-widgets!) (set! *damaged-widgets* '()))

  (define-class <widget> ()
    ((parent  initform: #f
              accessor: widget-parent)
     (visible initform: #t)
     (damaged initform: #t
              accessor: widget-damaged)
     (cursed  initform: #f
              accessor: widget-cursed)
     (x       initform: 0
              accessor: widget-x)
     (y       initform: 0
              accessor: widget-y)
     (cols    initform: 0
              accessor: widget-cols)
     (rows    initform: 0
              accessor: widget-rows)))

  (define-method (widget-damaged! (widget <widget>))
    (when (widget-visible? widget)
      (unless (memq widget *damaged-widgets*)
        (set! *damaged-widgets* (cons widget *damaged-widgets*)))
      (set! (widget-damaged (widget-root widget)) #t)))

  (define-method (widget-visible? (widget <widget>))
    (and (slot-value widget 'visible)
         (and (> (widget-cols widget) 0)
              (> (widget-rows widget) 0))
         (or (not (widget-parent widget))
             (widget-visible? (widget-parent widget)))))

  (define-method ((setter widget-visible) (widget <widget>) visible)
    (set! (slot-value widget 'visible) visible)
    (widget-damaged! widget))

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

  (define-method (print-widget! before: (widget <widget>) x y cols rows)
    (set! (widget-x    widget) x)
    (set! (widget-y    widget) y)
    (set! (widget-cols widget) cols)
    (set! (widget-rows widget) rows))

  (define-method (reprint-widget! (w <widget>))
    (print-widget! w (widget-x w) (widget-y w) (widget-cols w) (widget-rows w)))

  (define-method (print-widget! around: (widget <widget>) x y cols rows)
    (call-with-cursed call-next-method (widget-cursed widget)))

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

  ;;
  ;; Widget Wrap
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <widget-wrap> (<container>))

  (define (make-widget-wrap widget . kwargs)
    (let ((wrap (apply make <widget-wrap> kwargs)))
      (set! (widget-wrap-widget wrap) widget)
      wrap))

  (define-method (compute-layout (wrap <widget-wrap>) cols rows)
    (list (list (widget-wrap-widget wrap) 0 0 cols rows)))

  (define-method (widget-wrap-widget (wrap <widget-wrap>))
    (car (container-children wrap)))

  (define-method ((setter widget-wrap-widget) (wrap <widget-wrap>) (widget <widget>))
    (set! (widget-parent widget) wrap)
    (set! (container-children wrap) (list widget))
    (set! (widget-visible widget) #t)
    (widget-damaged! wrap))

  (define-method (widget-wrap-swap! (wrap <widget-wrap>) (widget <widget>))
    (let ((old (widget-wrap-widget wrap)))
      (set! (widget-wrap-widget wrap) widget)
      (set! (widget-visible old) #f)))

  ;;
  ;; Widget Stack
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <widget-stack> (<container>))

  (define (make-widget-stack root-widget #!rest widgets)
    (let ((stack (make <widget-stack> 'children (cons root-widget widgets))))
      (for-each (lambda (w) (set! (widget-parent w) stack))
                (cons root-widget widgets))
      stack))

  (define-method (compute-layout (stack <widget-stack>) cols rows)
    (list (list (car (container-children stack)) 0 0 cols rows)))

  (define-method (widget-stack-push! (stack <widget-stack>) (widget <widget>))
    (container-prepend-child! stack widget)
    (widget-damaged! stack))

  (define-method (widget-stack-pop! (stack <widget-stack>))
    (unless (null? (cdr (container-children stack)))
      (set! (container-children stack) (cdr (container-children stack)))
      (widget-damaged! stack)))

  (define-method  (widget-stack-peek (stack <widget-stack>))
    (let ((s (cdr (container-children stack))))
      (if (null? s) #f (car s)))))
