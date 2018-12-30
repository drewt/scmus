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

(import drewt.ncurses
        scmus.base
        scmus.client
        scmus.event
        scmus.option
        scmus.track
        scmus.tui
        scmus.view
        scmus.widgets)

(define *browser-location* (list "/"))

(define (browser-title-data)
  `((location . ,(car *browser-location*))))

(define (browser-location-push! str)
  (set! *browser-location* (cons str *browser-location*))
  (set! (format-text-data (frame-header (get-view 'browser)))
    (browser-title-data)))

(define (browser-location-pop!)
  (set! *browser-location* (cdr *browser-location*))
  (set! (format-text-data (frame-header (get-view 'browser)))
    (browser-title-data)))

(define browser-format
  (let ((dir (get-option 'format-browser-dir))
        (pla (get-option 'format-browser-playlist))
        (fil (get-option 'format-browser-file))
        (met (get-option 'format-browser-metadata)))
    (add-option-listener 'format-browser-dir      (lambda (o) (set! dir (option-value o))))
    (add-option-listener 'format-browser-playlist (lambda (o) (set! pla (option-value o))))
    (add-option-listener 'format-browser-file     (lambda (o) (set! fil (option-value o))))
    (add-option-listener 'format-browser-metadata (lambda (o) (set! met (option-value o))))
    (lambda (row)
      (case (window-row-type row)
        ((directory) dir)
        ((playlist)  pla)
        ((file)      fil)
        ((metadata)  met)))))

(define-class <browser-window> (<window>))

(define-method (widget-activate (window <browser-window>))
  (define (directory-activate! dir)
    (browser-location-push! (string-append "/" dir))
    (widget-stack-push! (widget-parent window)
      (make-browser-window (map (lambda (x) (make-window-row x (caar x) browser-format))
                              (scmus-lsinfo dir)))))
  (define (playlist-activate! playlist)
    (browser-location-push! (string-append "[" playlist "]"))
    (widget-stack-push! (widget-parent window)
      (make-browser-window (map (lambda (x) (make-window-row x 'file browser-format))
                                (scmus-list-playlist playlist)))))
  (define (file-activate! file)
    (browser-location-push! (string-append "/" (alist-ref 'file file)))
    (widget-stack-push! (widget-parent window)
      (make-browser-window (map (lambda (metadata)
                                  (make-window-row (list (cons 'tag (car metadata))
                                                         (cons 'value (cdr metadata)))
                                                   'metadata
                                                   browser-format))
                                (sort-metadata file)))))
  (unless (list-box-empty? window)
    (let ((selected (list-box-selected window)))
      (case (window-row-type selected)
        ((directory) (directory-activate! (cdar (window-row-data selected))))
        ((playlist)  (playlist-activate! (cdar (window-row-data selected))))
        ((file)      (file-activate! (window-row-data selected)))))))

(define-method (widget-deactivate (window <browser-window>))
  (let ((window (widget-parent window)))
    (when (widget-stack-peek window)
      (browser-location-pop!)
      (widget-stack-pop! window))))

(define-method (widget-add (window <browser-window>))
  (for-each (lambda (row)
              (case (window-row-type row)
                ((directory) (scmus-find-add! (cons 'base (cdar (window-row-data row)))))
                ((playlist)  (scmus-playlist-load! (cdar (window-row-data row))))
                ((file)      (scmus-add! (cdar (window-row-data row))))))
            (window-selected window))
  (scmus-update-queue!))

(define (make-browser-window data)
  (make <browser-window>
        'data       data
        'cursed     CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define-view browser
  (make-frame 'body   (make-widget-stack (make-browser-window '()))
              'header (make-format-text " Browser: ~{location}" (browser-title-data)
                                        'cursed CURSED-WIN-TITLE)))

(add-listener/global 'db-changed
  (lambda ()
    (set! (list-box-data (widget-last (frame-body (get-view 'browser))))
          (map (lambda (x) (make-window-row x (caar x) browser-format))
               (scmus-lsinfo "/")))))
