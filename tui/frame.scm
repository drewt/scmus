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
          scmus.tui.display
          scmus.tui.misc
          scmus.tui.widget)

  ;; TODO: This whole widget is an ugly hack.  This behaviour should be accomplished
  ;;       with a pile widget, in which the 'body' element is set to expand, and the
  ;;       header/footer elements are not.
  (define-class <frame> (<container>)
    ((body    initform: #f
              reader:   frame-body)
     (header  initform: #f
              reader:   frame-header)
     (footer  initform: #f
              reader:   frame-footer)
     (focused initform: 'body)))

  (define (make-frame . kwargs)
    (apply make <frame> kwargs))

  (define-method (widget-focus (frame <frame>))
    (widget-focus
      (case (slot-value frame 'focused)
        ((body)   (frame-body frame))
        ((header) (frame-header frame))
        ((footer) (frame-footer frame))
        (else (assert #f "widget-focus" frame)))))

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
