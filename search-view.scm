;;
;; Copyright 2014 Drew Thoreson
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

(declare (unit search-view)
         (uses editable ncurses scmus-client window ui-curses)
         (export make-search-view search-edit! search-clear! search-add!
                 search-remove!))

(define (search-result? row)
  (and (pair? row) (not (separator? row))))

(define (search-changed! . ignore)
  (register-event! 'search-changed))

(define (search-edit! window)
  (let ((selected (window-selected window)))
    (if (editable? selected)
      (set-input-mode! 'edit-mode
                       selected
                       (cons (+ 1 (- (window-sel-pos window)
                                     (window-top-pos window)))
                             3)))))

(define (add-search-field! window)
  (let-values (((queries results) (search-window-data window)))
    (*window-data-set! window (append queries
                                      (list (make-search-field)
                                            '(separator . ""))
                                      results))
    (window-data-len-set! window (+ 1 (window-data-len window)))
    (if (>= (window-sel-pos window) (length queries))
      (window-sel-pos-set! window (+ 1 (window-sel-pos window))))
    (search-changed!)))

(define (add-selected-tracks! window)
  (for-each (lambda (x) (if (pair? x) (scmus-add! (track-file x))))
            (window-all-selected window)))

(define (search-add! window)
  (let ((selected (window-selected window)))
    (cond
      ((editable? selected) (add-search-field! window))
      ((search-result? selected) (add-selected-tracks! window)))))

(define (search-remove! window)
  (let-values (((prev rest) (split-at (*window-data window)
                                      (window-sel-pos window))))
    (cond
      ((null? prev) (editable-clear! (car rest)))
      ((separator? (car rest)) (void))
      (else
        (*window-data-set! window (append prev (cdr rest)))
        (window-data-len-set! window (- (window-data-len window) 1))
        (window-sel-pos-set! window (min (- (window-data-len window) 1)
                                         (window-sel-pos window)))))
    (search-changed!)))

(define (search-clear! window)
  (let loop ((data (window-data window)) (result '()))
    (if (or (null? data) (search-result? (car data)))
      (begin
        (*window-data-set! window (reverse result))
        (window-data-len-update! window)
        (window-sel-pos-set! window
                             (min (window-sel-pos window)
                                  (- (window-data-len window) 1))))
      (loop (cdr data) (cons (car data) result))))
  (search-changed!))

(define (search-window-data window)
  (let loop ((data (window-data window)) (result '()))
    (if (not (editable? (car data)))
      (values (reverse result) (cdr data))
      (loop (cdr data) (cons (car data) result)))))

(define (string->tag str)
  (let ((tag (string->symbol (string-downcase str))))
    (if (memv tag
            '(artist album albumartist title tracknumber name genre date
              composer performer comment discnumber))
      tag
      #f)))

(define (parse-constraint str)
  (let ((index (string-index str #\:)))
    (if index
      (let ((tag (string->tag (string-take str index))))
        (if tag
          (cons tag (string-drop str (+ 1 index)))
          (cons 'any str)))
      (cons 'any str))))

(define (search-activate! window)
  (define (gather-constraints)
    (map (lambda (e) (parse-constraint (editable-text e)))
         (remove (lambda (x) (= 0 (editable-length x)))
                 (search-window-data window))))
  (let ((results (apply scmus-search-songs #f #f (gather-constraints))))
    (*window-data-set! window (append (window-data window) results))
    (window-data-len-set! window (+ (window-data-len window)
                                    (length results))))
  (search-changed!))

(define (make-search-field)
  (make-simple-editable (lambda (e) #t)
                        (lambda (e) (set-input-mode! 'normal-mode))
                        search-changed!))

(define (search-match row query)
  (and (pair? row) (track-match row query)))

(define (search-window-print-row window row line-nr cursed)
  (cond
    ((editable? row)
      (format-print-line line-nr " * ~a" (editable-text row)))
    ((separator? row) (move line-nr 0) (clrtoeol))
    (else
      (track-print-line line-nr (get-format 'format-library) row cursed))))

(define (make-search-view)
  (make-view (make-window (list (make-search-field) '(separator . ""))
                          *window-data
                          (lambda (w) (search-changed!))
                          search-activate!
                          void
                          search-match)
             "Search"
             search-window-print-row))
