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

(declare (unit browser-view)
         (uses ncurses event options client track ui-lib view window)
         (export))

(import ncurses)
(import scmus.base scmus.client scmus.event scmus.track scmus.window)

(: browser-add! (* -> undefined))
(define (browser-add! selected)
  (case (car selected)
    ((directory) (scmus-find-add! (cons 'base (cdadr selected))))
    ((playlist)  (scmus-playlist-load! (cdadr selected)))
    ((file)      (scmus-add! (cdadr selected)))))

(: browser-add-selected! (window -> undefined))
(define (browser-add-selected! window)
  (for-each browser-add! (window-all-selected window))
  (scmus-update-queue!))

(: browser-match (* string -> boolean))
(define (browser-match row query)
  (case (car row)
    ((directory playlist) (substring-match (cdadr row) query))
    ((file) (track-match (cdr row) query))
    (else #f)))

(: browser-format (symbol -> format-spec))
(define (browser-format type)
  (case type
    ((directory) (get-format 'format-browser-dir))
    ((playlist)  (get-format 'format-browser-playlist))
    ((file)      (get-format 'format-browser-file))
    ((metadata)  (get-format 'format-browser-metadata))))

(: tag-data (list -> (list-of (pair symbol *))))
(define (tag-data data)
  (if (null? data)
    data
    (map (lambda (x) (cons (caar x) x)) data)))

(define *browser-location* (list "/"))

(: browser-activate! (window -> undefined))
(define (browser-activate! window)
  (let ((selected (window-selected window)))
    (case (car selected)
      ((directory) (directory-activate! window (cdadr selected)))
      ((playlist)  (playlist-activate! window (cdadr selected)))
      ((file)      (file-activate! window (cdr selected))))))

(: directory-activate! (window string -> undefined))
(define (directory-activate! window dir)
  (set! *browser-location*
    (cons (string-append "/" dir) *browser-location*))
  (window-stack-push! (widget-parent window)
    (make-browser-window (tag-data (scmus-lsinfo dir)))))

(: playlist-activate! (window string -> undefined))
(define (playlist-activate! window playlist)
  (set! *browser-location*
    (cons (string-append "[" playlist "]") *browser-location*))
  (window-stack-push! (widget-parent window)
    (make-browser-window (tag-data (scmus-list-playlist playlist)))))

(: file-activate! (window track -> undefined))
(define (file-activate! window file)
   (define (format-metadata metadata)
    (map (lambda (x)
           (cons 'metadata
                 (list (cons 'tag   (car x))
                       (cons 'value (cdr x)))))
         metadata))
  (set! *browser-location*
    (cons (string-append "/" (alist-ref 'file file)) *browser-location*))
  (window-stack-push! (widget-parent window)
    (make-browser-window (format-metadata (sort-metadata file)))))

(: browser-deactivate (window -> undefined))
(define (browser-deactivate! window)
  (let ((window (widget-parent window)))
    (when (window-stack-peek window)
      (set! *browser-location* (cdr *browser-location*))
      (window-stack-pop! window))))

(define (browser-title-data view)
  `((location . ,(car *browser-location*))))

(define-event-handler (db-changed) ()
  (set! (*window-data (widget-last (view-widget (get-view 'browser))))
        (tag-data (scmus-lsinfo "/"))))

(define (make-browser-window data)
  (make-window 'data       data
               'activate   browser-activate!
               'deactivate browser-deactivate!
               'match      browser-match
               'add        browser-add-selected!
               'format     browser-format))

(define-view browser
  (make-view (make-window-stack (make-browser-window '()))
             " Browser: ~{location}"
             'title-data browser-title-data))
