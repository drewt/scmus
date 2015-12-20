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
         (export browser-add-selected! make-browser-view update-browser!))

(: browser-window-data (window -> list))
(define (browser-window-data window)
  (cdr (*window-data window)))

(: browser-prev-window (window -> window))
(define (browser-prev-window window)
  (car (*window-data window)))

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

(: directory-activate! (window string -> undefined))
(define (directory-activate! window dir)
  (set-window! 'browser (make-browser-window window (scmus-lsinfo dir)))
  (register-event! 'browser-data-changed))

(: playlist-activate! (window string -> undefined))
(define (playlist-activate! window playlist)
  (set-window! 'browser (make-browser-window window (scmus-list-playlist playlist)))
  (register-event! 'browser-data-changed))

(: file-activate! (window list -> undefined))
(define (file-activate! window file)
  (set-window! 'browser (make-browser-window window (sort-metadata file)))
  (register-event! 'browser-data-changed))

(: browser-activate! (window -> undefined))
(define (browser-activate! window)
  (let ((selected (window-selected window)))
    (if (pair? (car selected))
      (case (caar selected)
        ((directory) (directory-activate! window (cdar selected)))
        ((playlist) (playlist-activate! window (cdar selected)))
        ((file) (file-activate! window selected))))))

(: browser-deactivate! (window -> undefined))
(define (browser-deactivate! window)
  (when (browser-prev-window window)
    (set-window! 'browser (browser-prev-window window))
    (register-event! 'browser-data-changed)))

(: browser-match (* string -> boolean))
(define (browser-match row query)
  (if (pair? (car row))
    (case (caar row)
      ((directory playlist) (substring-match (cdar row) query))
      ((file) (track-match row query))
      (else #f))
    #f))

(: browser-window-print-row (window * fixnum fixnum -> undefined))
(define (browser-window-print-row window row line-nr cursed)
  (if (pair? (car row))
    (case (caar row)
      ((directory) (track-print-line line-nr
                                     (get-format 'format-browser-dir)
                                     row
                                     cursed))
      ((playlist)  (track-print-line line-nr
                                     (get-format 'format-browser-playlist)
                                     row
                                     cursed))
      ((file)      (track-print-line line-nr
                                     (get-format 'format-browser-file)
                                     row
                                     cursed)))
    (alist-print-line window row line-nr cursed)))

(: make-browser-window ((or window boolean) list -> window))
(define (make-browser-window prev-win data)
  (make-window data:       (cons prev-win data)
               data-thunk: browser-window-data
               changed:    (lambda (w) (register-event! 'browser-changed))
               activate:   browser-activate!
               deactivate: browser-deactivate!
               match:      browser-match))

(: update-browser! thunk)
(define (update-browser!)
  (set-window! 'browser (make-browser-window #f (scmus-lsinfo "/")))
  (register-event! 'browser-data-changed))

(define-view browser
  (make-view (make-browser-window #f (scmus-lsinfo "/"))
             "Browser"
             print-line: browser-window-print-row
             add:        browser-add-selected!))

(define-event-handler (browser-changed)
  (update-view! 'browser))

(define-event-handler (browser-data-changed)
  (window-data-len-update! (get-window 'browser))
  (update-view! 'browser))
