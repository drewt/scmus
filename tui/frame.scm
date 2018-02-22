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
    ((widget initform: #f
             accessor: frame-widget)
     (title  initform: (make-text "")
             accessor: frame-title)))

  (define-method (initialize-instance (frame <frame>))
    (call-next-method)
    (when (frame-widget frame)
      (set! (widget-parent (frame-widget frame)) frame))
    (when (frame-title frame)
      (set! (widget-parent (frame-title frame)) frame)))

  (define (make-frame widget title . kwargs)
    (apply make <frame> 'widget widget
                        'title title
                        kwargs))

  (define-method (widget-focus (frame <frame>))
    (widget-focus (frame-widget frame)))

  (define-method (container-children (frame <frame>))
    (append (if (frame-title frame)  (list (frame-title frame))  '())
            (if (frame-widget frame) (list (frame-widget frame)) '())))

  (define-method (compute-layout (frame <frame>) cols rows)
    (list (list (frame-title frame)  0 0 cols 1)
          (list (frame-widget frame) 0 1 cols (- rows 1)))))
