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

(declare (export))

(import drewt.ncurses)
(import scmus.base scmus.client scmus.event scmus.track scmus.tui scmus.widgets)

(define *browser-location* (list "/"))

(define (browser-location-push! str)
  (set! *browser-location* (cons str *browser-location*))
  (set! (format-text-data (frame-header (get-view 'browser)))
    (browser-title-data)))

(define (browser-location-pop!)
  (set! *browser-location* (cdr *browser-location*))
  (set! (format-text-data (frame-header (get-view 'browser)))
    (browser-title-data)))

(define (browser-format row)
  (case (window-row-type row)
    ((directory) (get-format 'format-browser-dir))
    ((playlist)  (get-format 'format-browser-playlist))
    ((file)      (get-format 'format-browser-file))
    ((metadata)  (get-format 'format-browser-metadata))))

(define (browser-activate! window)
  (let ((selected (window-selected window)))
    (case (window-row-type selected)
      ((directory) (directory-activate! window (cdar (window-row-data selected))))
      ((playlist)  (playlist-activate! window (cdar (window-row-data selected))))
      ((file)      (file-activate! window (window-row-data selected))))))

(define (directory-activate! window dir)
  (browser-location-push! (string-append "/" dir))
  (widget-stack-push! (widget-parent window)
    (make-browser-window (map (lambda (x) (make-window-row x (caar x) browser-format))
                              (scmus-lsinfo dir)))))

(define (playlist-activate! window playlist)
  (browser-location-push! (string-append "[" playlist "]"))
  (widget-stack-push! (widget-parent window)
    (make-browser-window (map (lambda (x) (make-window-row x 'file browser-format))
                              (scmus-list-playlist playlist)))))

(define (file-activate! window file)
  (browser-location-push! (string-append "/" (alist-ref 'file file)))
  (widget-stack-push! (widget-parent window)
    (make-browser-window (map (lambda (metadata)
                                (make-window-row (list (cons 'tag (car metadata))
                                                       (cons 'value (cdr metadata)))
                                                 'metadata
                                                 browser-format))
                              file))))

(define (browser-deactivate! window)
  (let ((window (widget-parent window)))
    (when (widget-stack-peek window)
      (browser-location-pop!)
      (widget-stack-pop! window))))

(define (browser-match row query)
  (case (window-row-type row)
    ((directory playlist) (substring-match (cdar (window-row-data row)) query))
    ((file) (track-match (window-row-data row) query))
    (else #f)))

(define (browser-add-selected! window)
  (for-each (lambda (row)
              (case (window-row-type row)
                ((directory) (scmus-find-add! (cons 'base (cdar (window-row-data row)))))
                ((playlist)  (scmus-playlist-load! (cdar (window-row-data row))))
                ((file)      (scmus-add! (cdar (window-row-data row))))))
            (window-all-selected window))
  (scmus-update-queue!))

(define (browser-title-data)
  `((location . ,(car *browser-location*))))

(define (make-browser-window data)
  (make-window 'data       data
               'activate   browser-activate!
               'deactivate browser-deactivate!
               'match      browser-match
               'add        browser-add-selected!
               'cursed     CURSED-WIN
               'cursed-fn  (win-cursed-fn)))

(define-view browser
  (make-frame 'body   (make-widget-stack (make-browser-window '()))
              'header (make-format-text " Browser: ~{location}" (browser-title-data)
                                        'cursed CURSED-WIN-TITLE)))

(define-event-handler (db-changed) ()
  (set! (window-data (widget-last (frame-body (get-view 'browser))))
        (map (lambda (x) (make-window-row x (caar x) browser-format))
             (scmus-lsinfo "/"))))
