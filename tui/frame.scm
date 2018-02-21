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

  ; FIXME: this should be a container
  (define-class <frame> (<widget>)
    ((widget initform: #f
             accessor: frame-widget)
     (title  initform: (make-text "")
             accessor: frame-title)))

  (define-method (initialize-instance (frame <frame>))
    (call-next-method)
    (set! (widget-parent (frame-widget frame)) frame))

  (define (make-frame widget title . kwargs)
    (apply make <frame> 'widget widget
                        'title title
                        kwargs))

  (define-method (widget-focus (frame <frame>))
    (widget-focus (frame-widget frame)))

  (define-method (widget-geometry-set! (frame <frame>) cols rows)
    (widget-geometry-set! (frame-title frame) cols 1)
    (widget-geometry-set! (frame-widget frame) cols (max 0 (- rows 1))))

  (define-method (print-widget! (frame <frame>) x y cols rows)
    (print-widget! (frame-title frame) x y cols 1)
    (when (> rows 1)
      (print-widget! (frame-widget frame) x (+ 1 y) cols (- rows 1)))))
