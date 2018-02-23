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

(module scmus.tui.frame *
  (import coops
          scmus.base
          scmus.format
          scmus.tui.display
          scmus.tui.misc
          scmus.tui.widget)

  (define-class <frame> (<container>)
    ((body   initform: #f
             accessor: frame-body)
     (header initform: #f
             accessor: frame-header)
     (footer initform: #f
             accessor: frame-footer)))

  (define-method (initialize-instance (frame <frame>))
    (call-next-method)
    (when (frame-body frame)
      (set! (widget-parent (frame-body frame)) frame))
    (when (frame-header frame)
      (set! (widget-parent (frame-header frame)) frame))
    (when (frame-footer frame)
      (set! (widget-parent (frame-footer frame)) frame)))

  (define (make-frame . kwargs)
    (apply make <frame> kwargs))

  (define-method (widget-focus (frame <frame>))
    (if (frame-body frame)
      (widget-focus (frame-body frame))
      #f))

  (define-method (container-children (frame <frame>))
    (let ((head (frame-header frame))
          (body (frame-body frame))
          (foot (frame-footer frame)))
      (append (if head (list head) '())
              (if body (list body) '())
              (if foot (list foot) '()))))

  ;; Nasty imperative code. Header and footer will consume the entire frame
  ;; if they want to.  Maybe we could have an option to limit their size?
  (define-method (compute-layout (frame <frame>) cols rows)
    (let ((title  (frame-header frame))
          (body   (frame-body   frame))
          (footer (frame-footer frame))
          (x      0))
      (when footer
        (let-values (((_ foot-rows) (widget-size footer cols rows)))
          (set! footer (list footer 0 (- rows foot-rows) cols foot-rows))
          (set! rows (- rows foot-rows))))
      (when title
        (let-values (((_ head-rows) (widget-size title cols rows)))
          (set! title (list title 0 0 cols head-rows))
          (set! rows (- rows head-rows))
          (set! x (+ x head-rows))))
      (when body
        (set! body (list body 0 x cols rows)))
      (append (if title  (list title)  '())
              (if body   (list body)   '())
              (if footer (list footer) '())))))
