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
  (if (pair? (car selected))
    (case (caar selected)
      ((directory) (scmus-find-add! (cons 'base (cdar selected))))
      ((playlist)  (scmus-playlist-load! (cdar selected)))
      ((file)      (scmus-add! (cdar selected))))))

(: browser-add-selected! (window -> undefined))
(define (browser-add-selected! window)
  (for-each browser-add! (window-all-selected window)))

(: browser-match (* string -> boolean))
(define (browser-match row query)
  (if (pair? (car row))
    (case (caar row)
      ((directory playlist) (substring-match (cdar row) query))
      ((file) (track-match row query))
      (else #f))
    #f))

(: browser-print-line (window * fixnum -> string))
(define (browser-print-line window row nr-cols)
  (define (format-string type)
    (case type
      ((directory) (get-format 'format-browser-dir))
      ((playlist)  (get-format 'format-browser-playlist))
      ((file)      (get-format 'format-browser-file))))
  (if (pair? (car row))
    (scmus-format (format-string (caar row)) nr-cols row)
    (alist-print-line window row nr-cols)))

(: update-browser! thunk)
(define (update-browser!)
  (let ((window (get-window 'browser)))
    (let loop ()
      (when (window-stack-peek window)
        (window-stack-pop! window)
        (loop)))
    (set! (*window-data window) #f)
    (browser-get-data window))
  (register-event! 'browser-data-changed))

(: browser-get-data (window -> list))
(define (browser-get-data window)
  (unless (*window-data window)
    (set! (*window-data window) (scmus-lsinfo "/"))
    (window-data-len-update! window))
  (*window-data window))

(: browser-activate! (window -> undefined))
(define (browser-activate! window)
  (let ((selected (window-selected window)))
    (if (pair? (car selected))
      (case (caar selected)
        ((directory) (directory-activate! window (cdar selected)))
        ((playlist) (playlist-activate! window (cdar selected)))
        ((file) (file-activate! window selected))))))

(define (directory-activate! window dir)
  (window-stack-push! window (scmus-lsinfo dir) browser-get-data)
  (register-event! 'browser-data-changed))

(define (playlist-activate! window playlist)
  (window-stack-push! window (scmus-list-playlist playlist) browser-get-data)
  (register-event! 'browser-data-changed))

(define (file-activate! window file)
  (window-stack-push! window (sort-metadata file) browser-get-data)
  (register-event! 'browser-data-changed))

(define (browser-deactivate! window)
  (when (window-stack-peek window)
    (window-stack-pop! window)
    (register-event! 'browser-data-changed)))

(define (browser-changed! window)
  (register-event! 'browser-changed))

(define-view browser
  (make-view (make-stack-window 'data #f
                                'data-thunk browser-get-data
                                'activate   browser-activate!
                                'deactivate browser-deactivate!
                                'changed    browser-changed!
                                'match      browser-match
                                'print-line browser-print-line)
             " Browser"
             add: browser-add-selected!))

(define-event-handler (browser-changed)
  (update-view! 'browser))

(define-event-handler (browser-data-changed)
  (window-data-len-update! (get-window 'browser))
  (update-view! 'browser))
