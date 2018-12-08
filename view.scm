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

(declare (hide *view-ctors*))

(import coops-utils
        scmus.base
        scmus.keys
        scmus.tui
        scmus.widgets)

;; Custom <widget-wrap> which keeps an alist of widgets, and allows swapping
;; the active widget by name.
(define-class <widget-bag> (<widget-wrap>)
  ((widgets initform: '()
            accessor: widget-bag-widgets)
   (active  initform: 'none
            reader:   widget-bag-active)))

(define (make-widget-bag widgets active . kwargs)
  (apply make <widget-bag> 'widgets widgets 'active active kwargs))

(define-method (initialize-instance (bag <widget-bag>))
  (call-next-method)
  (for-each (lambda (n/w)
              (set! (widget-parent (cdr n/w)) bag)
              (set! (widget-visible (cdr n/w)) #f))
            (widget-bag-widgets bag))
  (set! (widget-wrap-widget bag)
    (alist-ref (widget-bag-active bag) (widget-bag-widgets bag))))

(define-method ((setter widget-bag-active) (bag <widget-bag>) name)
  (let ((widget (alist-ref name (widget-bag-widgets bag))))
    (when widget
      (widget-wrap-swap! bag widget)
      (set! (slot-value bag 'active) name))))

(define-method (widget-bag-add! (bag <widget-bag>) (widget <widget>) name)
  (set! (widget-bag-widgets bag)
    (cons (cons name widget)
          (widget-bag-widgets bag))))

(define-method (widget-bag-ref (bag <widget-bag>) name)
  (alist-ref name (widget-bag-widgets bag)))

(define-method (handle-input (bag <widget-bag>) input)
  (normal-mode-key (widget-bag-active bag) input))

(define view-widget (make-widget-bag '() 'none))

(: *view-ctors* (list-of (pair symbol (-> frame))))
(define *view-ctors* '())

(: register-view! (symbol (-> frame) -> undefined))
(define (register-view! name ctor)
  (register-context! name)
  (set! *view-ctors* (cons (cons name ctor) *view-ctors*)))

(: init-views! thunk)
(define (init-views!)
  (for-each (lambda (x)
              (widget-bag-add! view-widget ((cdr x)) (car x)))
            *view-ctors*))

(: get-view (symbol -> frame))
(define (get-view name)
  (widget-bag-ref view-widget name))

(: current-view (-> frame))
(define (current-view)
  (widget-wrap-widget view-widget))

(: current-view-name (-> symbol))
(define (current-view-name)
  (widget-bag-active view-widget))

(: current-view? (symbol -> boolean))
(define (current-view? view-name)
  (eq? (widget-bag-ref view-widget view-name)
       (widget-wrap-widget view-widget)))

(: set-view! (symbol -> undefined))
(define (set-view! view-name)
  (when (widget-bag-ref view-widget view-name)
    (set! (widget-bag-active view-widget) view-name)))
