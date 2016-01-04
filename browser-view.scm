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

(declare (unit browser-view)
         (uses event ncurses option scmus-client ui-lib view window)
         (export update-browser!))

(: browser-add! (* -> undefined))
(define (browser-add! selected)
  (case (caar selected)
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

(: browser-print-line (window * fixnum -> string))
(define (browser-print-line window row nr-cols)
  (define (format-string type)
    (case type
      ((directory) (get-format 'format-browser-dir))
      ((playlist)  (get-format 'format-browser-playlist))
      ((file)      (get-format 'format-browser-file))
      ((metadata)  (get-format 'format-browser-metadata))))
  (scmus-format (format-string (car row)) nr-cols (cdr row)))

(: update-browser! thunk)
(define (update-browser!)
  (let ((window (get-window 'browser)))
    (let loop ()
      (when (window-stack-peek window)
        (window-stack-pop! window)
        (loop)))
    (set! (*window-data window) #f)
    (browser-get-data window)
    (void)))

(: tag-data (list -> (list-of (pair symbol *))))
(define (tag-data data)
  (if (null? data)
    data
    (map (lambda (x) (cons (caar x) x)) data)))

(: browser-get-data (window -> list))
(define (browser-get-data window)
  (unless (*window-data window)
    (set! (*window-data window) (tag-data (scmus-lsinfo "/"))))
  (*window-data window))

(: browser-activate! (window -> undefined))
(define (browser-activate! window)
  (let ((selected (window-selected window)))
    (case (car selected)
      ((directory) (directory-activate! window (cdadr selected)))
      ((playlist)  (playlist-activate! window (cdadr selected)))
      ((file)      (file-activate! window (cdr selected))))))

(define (directory-activate! window dir)
  (window-stack-push! window (tag-data (scmus-lsinfo dir)) browser-get-data))

(define (playlist-activate! window playlist)
  (window-stack-push! window (tag-data (scmus-list-playlist playlist)) browser-get-data))

(define (file-activate! window file)
  (define (format-metadata metadata)
    (map (lambda (x)
           (cons 'metadata
                 (list (cons 'tag   (car x))
                       (cons 'value (cdr x)))))
         metadata))
  (window-stack-push! window (format-metadata (sort-metadata file)) browser-get-data))

(define (browser-deactivate! window)
  (when (window-stack-peek window)
    (window-stack-pop! window)))

(define-view browser
  (make-view (make-stack-window 'data #f
                                'data-thunk browser-get-data
                                'activate   browser-activate!
                                'deactivate browser-deactivate!
                                'match      browser-match
                                'add        browser-add-selected!
                                'print-line browser-print-line)
             " Browser"))
