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

(declare (hide))

(import coops-utils
        drewt.ncurses
        scmus.base
        scmus.client
        scmus.format
        scmus.option
        scmus.track
        scmus.tui
        scmus.view
        scmus.widgets)

(add-option-listener 'format-search-file
  (lambda (_) (widget-invalidate search-widget)))

(define (make-search-field)
  (make-text-input "" " * " 'on-commit search-field-commit!))

(define (search-field-commit! widget)
  (void))

(define-class <search-window> (<window>))

(define-method (widget-activate (window <search-window>))
  (define (string->tag str)
    (string->symbol (string-trim-both (string-downcase str))))
  (define (search-field->constraint w)
    (let* ((str (text-input-get-text w))
           (index (string-index str #\:)))
      (cond
        ((string=? str "") #f)
        (index
          (cons (string->tag (string-take str index))
                (string-trim-both (string-drop str (+ 1 index)))))
        (else
          (cons 'any (string-trim-both str))))))
  (define (constraints)
    (filter values
            (map search-field->constraint
                 (filter (lambda (row) (instance-of? row <text-input>))
                         (vector->list (list-box-data window))))))
  (let ((results (apply scmus-search-songs #f #f (constraints))))
    (set! (list-box-data window) (append (vector->list (list-box-data window))
                                         (map (lambda (track)
                                                (make-window-row track 'file 'format-search-file))
                                              results)))))

(define-method (widget-edit (window <search-window>))
  (let ((selected (list-box-selected window)))
    (when (instance-of? selected <text-input>)
      (text-input-begin selected steal-focus: #t))))

(define-method (widget-add (window <search-window>))
  (let ((selected (list-box-selected window)))
    (cond
      ((instance-of? selected <text-input>)
        (add-search-field! window))
      ((instance-of? selected <window-row>)
        (add-selected-tracks! window)))))

(define (search-window-data window)
  (let loop ((data (vector->list (list-box-data window))) (result '()))
    (if (or (null? data) (instance-of? (car data) <window-separator>))
      (values (reverse result) (car data) (cdr data))
      (loop (cdr data) (cons (car data) result)))))

(define (add-search-field! window)
  (let-values (((queries separator results) (search-window-data window)))
    (set! (list-box-data window)
      (append queries
              (list (make-search-field) separator)
              results))
    (when (>= (list-box-sel-pos window) (length queries))
      (widget-move window 1 #f))))

(define (add-selected-tracks! window)
  (for-each (lambda (row)
              (when (instance-of? row <window-row>)
                (scmus-add! (track-file (window-row-data row)))))
            (window-selected window)))

(define-method (widget-remove (window <search-window>))
  (let-values (((prev rest) (split-at (vector->list (list-box-data window))
                                      (list-box-sel-pos window))))
    ; FIXME: this should remove all marked rows, not just the selected row
    (cond
      ; if there's only one search field, we clear it instead of removing it
      ((and (null? prev)
            (instance-of? (cadr rest) <window-separator>))
        (text-input-set-text! (car rest) ""))
      ((not (instance-of? (car rest) <window-separator>))
        (set! (list-box-data window) (append prev (cdr rest)))))))

(define-method (widget-clear (window <search-window>))
  (let loop ((data (vector->list (list-box-data window))) (result '()))
    (if (or (null? data) (instance-of? (car data) <window-row>))
      (set! (list-box-data window) (reverse result))
      (loop (cdr data) (cons (car data) result)))))

(define search-widget
  (make <search-window>
        'data       (list (make-search-field)
                          (make <window-separator>
                                'text " Results"
                                'cursed CURSED-WIN-TITLE))
        'cursed     CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define-view search
  (make-frame 'body   search-widget
              'header (make-text " Search" 'cursed CURSED-WIN-TITLE)))
