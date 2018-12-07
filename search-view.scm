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

(declare (export make-search-view search-edit! search-clear! search-add!
                 search-remove!))

(import coops-utils
        drewt.ncurses)
(import scmus.base
        scmus.client
        scmus.format
        scmus.track
        scmus.tui
        scmus.widgets)

(define (search-format row)
  (get-format 'format-search-file))

(define (search-activate! window)
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
                         (window-data window)))))
  (let ((results (apply scmus-search-songs #f #f (constraints))))
    (set! (window-data window) (append (window-data window)
                                       (map (lambda (track)
                                              (make-window-row track 'file search-format))
                                            results)))))

(define (search-match row query)
  (and (instance-of? row <window-row>)
       (track-match (window-row-data row) query)))

(define (search-add! window)
  (let ((selected (window-selected window)))
    (cond
      ((instance-of? selected <text-input>)
        (add-search-field! window))
      ((instance-of? selected <window-row>)
        (add-selected-tracks! window)))))

(define (search-window-data window)
  (let loop ((data (window-data window)) (result '()))
    (if (or (null? data) (instance-of? (car data) <window-separator>))
      (values (reverse result) (car data) (cdr data))
      (loop (cdr data) (cons (car data) result)))))

(define (add-search-field! window)
  (let-values (((queries separator results) (search-window-data window)))
    (set! (window-data window)
      (append queries
              (list (make-search-field) separator)
              results))
    (when (>= (window-sel-pos window) (length queries))
      (window-move-down! window 1))))

(define (add-selected-tracks! window)
  (for-each (lambda (row)
              (when (instance-of? row <window-row>)
                (scmus-add! (track-file (window-row-data row)))))
            (window-all-selected window)))

(define (search-remove! window)
  (let-values (((prev rest) (split-at (window-data window)
                                      (window-sel-pos window))))
    ; FIXME: this should remove all marked rows, not just the selected row
    (cond
      ; if there's only one search field, we clear it instead of removing it
      ((and (null? prev)
            (instance-of? (cadr rest) <window-separator>))
        (text-input-set-text! (car rest) ""))
      ((not (instance-of? (car rest) <window-separator>))
        (set! (window-data window) (append prev (cdr rest)))))))

(define (search-clear! window)
  (let loop ((data (window-data window)) (result '()))
    (if (or (null? data) (instance-of? (car data) <window-row>))
      (set! (window-data window) (reverse result))
      (loop (cdr data) (cons (car data) result)))))

(define (search-edit! window)
  (let ((selected (window-selected window)))
    (when (instance-of? selected <text-input>)
      (text-input-begin selected steal-focus: #t))))

(define (make-search-field)
  (make-text-input "" "* " 'on-commit search-field-commit!))

(define (search-field-commit! widget)
  (void))

(define-view search
  (make-frame 'body   (make-window 'data       (list (make-search-field)
                                                     (make <window-separator>
                                                           'text "Results"
                                                           'cursed CURSED-WIN-TITLE))
                                   'activate   search-activate!
                                   'match      search-match
                                   'add        search-add!
                                   'remove     search-remove!
                                   'clear      search-clear!
                                   'edit       search-edit!
                                   'cursed     CURSED-WIN
                                   'cursed-fn  (win-cursed-fn))
              'header (make-text " Search" 'cursed CURSED-WIN-TITLE)))
