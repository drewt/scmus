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

  (define-syntax define-abstract-method
    (syntax-rules ()
      ((define-abstract-method (name . args))
        (define-method (name . args)
          (abort
            (make-composite-condition
              (make-property-condition 'exn
                'message "Abstract method not implemented by subclass"
                'arguments '(name . args))
              (make-property-condition 'coops)))))))

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

  (define-method ((setter widget-cursed) after: (widget <widget>))
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

  (define-method (widget-size (widget <widget>) available-cols available-rows)
    (values available-cols available-rows))

  (define-method (print-widget! before: (widget <widget>) x y cols rows)
    (set! (widget-x    widget) x)
    (set! (widget-y    widget) y)
    (set! (widget-cols widget) cols)
    (set! (widget-rows widget) rows))

  (define-method (reprint-widget! (w <widget>))
    (print-widget! w (widget-x w) (widget-y w) (widget-cols w) (widget-rows w)))

  (define-method (print-widget! around: (widget <widget>) x y cols rows)
    (call-with-cursed call-next-method (widget-cursed widget)))

  ;; Input handler.  If the widget doesn't handle the input, it should invoke
  ;; CALL-NEXT-METHOD to allow the superclass to handle the input.  If the input
  ;; is handled, HANDLE-INPUT should return #t so that the default handler is
  ;; not called.
  (define-method (handle-input (widget <widget>) input)
    (let ((parent (widget-parent widget)))
      (if parent
        (handle-input parent input)
        #f)))

  ;;
  ;; Container
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <container> (<widget>)
    ((focus initform: #f
            accessor: container-focus)))

  ;; Ensure that WIDGET-PARENT is set correctly on all children.
  (define-method (initialize-instance (container <container>))
    (call-next-method)
    (for-each (lambda (child)
                (set! (widget-parent child) container))
              (container-children container)))

  ;; Method returning a list of the container's child widgets.
  (define-abstract-method (container-children (container <container>)))

  ;; Method returning a list of lists: (WIDGET X Y COLS ROWS) where:
  ;;   * WIDGET is a child widget
  ;;   * X is the X coordinate of WIDGET (relative to the container's X,Y coordinates)
  ;;   * Y is the Y coordinate of WIDGET (relative to the container's X,Y coordinates)
  ;;   * COLS is the number of columns allocated to WIDGET
  ;;   * ROWS is the number of rows allocated to WIDGET
  ;;
  ;; This method is called in the generic PRINT-WIDGET! implementation for containers.
  ;; It is not necessary to implement this method in a subclass that overrides
  ;; PRINT-WIDGET!.
  (define-abstract-method (compute-layout (container <container>) cols rows))

  (define-method (widget-first (container <container>))
    (widget-first (car (container-children container))))

  (define-method (widget-last (container <container>))
    (widget-last (car (reverse (container-children container)))))

  (define-method (widget-focus (container <container>))
    (let ((focus (container-focus container)))
      (if focus
        (widget-focus focus)
        #f)))

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

  (define-class <widget-wrap> (<container>)
    ((widget initform: #f
             reader:   widget-wrap-widget)))

  (define (make-widget-wrap widget . kwargs)
    (apply make <widget-wrap> 'widget widget kwargs))

  (define-method ((setter widget-wrap-widget) (wrap <widget-wrap>) (widget <widget>))
    (set! (slot-value wrap 'widget) widget)
    (when widget
      (set! (widget-parent widget) wrap)
      (set! (widget-visible widget) #t))
    (widget-damaged! wrap))

  (define-method (widget-focus (wrap <widget-wrap>))
    (widget-wrap-widget wrap))

  (define-method (widget-wrap-swap! (wrap <widget-wrap>) (widget <widget>))
    (let ((old (widget-wrap-widget wrap)))
      (set! (widget-wrap-widget wrap) widget)
      (when old (set! (widget-visible old) #f))))

  (define-method (container-children (wrap <widget-wrap>))
    (let ((widget (widget-wrap-widget wrap)))
      (if widget (list widget) '())))

  (define-method (compute-layout (wrap <widget-wrap>) cols rows)
    (let ((widget (widget-wrap-widget wrap)))
      (if widget
        (list (list (widget-wrap-widget wrap) 0 0 cols rows))
        '())))

  ;;
  ;; Widget Stack
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-class <widget-stack> (<container>)
    ((stack initform: '()
            reader:   container-children)))

  (define (make-widget-stack root-widget #!rest widgets)
    (make <widget-stack> 'stack (cons root-widget widgets)))

  (define-method (widget-focus (stack <widget-stack>))
    (car (container-children stack)))

  (define-method (compute-layout (stack <widget-stack>) cols rows)
    (list (list (car (container-children stack)) 0 0 cols rows)))

  (define-method (widget-stack-push! (stack <widget-stack>) (widget <widget>))
    (set! (slot-value stack 'stack)
      (cons widget (slot-value stack 'stack)))
    (set! (widget-parent widget) stack)
    (widget-damaged! stack))

  (define-method (widget-stack-pop! (stack <widget-stack>))
    (unless (null? (cdr (container-children stack)))
      (set! (slot-value stack 'stack)
        (cdr (slot-value stack 'stack)))
      (widget-damaged! stack)))

  (define-method  (widget-stack-peek (stack <widget-stack>))
    (let ((s (cdr (container-children stack))))
      (if (null? s) #f (car s)))))
