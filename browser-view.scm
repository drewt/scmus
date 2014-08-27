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
         (uses ncurses scmus-client ui-curses window)
         (export browser-add-selected! make-browser-view update-browser!))

(define (browser-window-data window)
  (cdr (*window-data window)))

(define (browser-prev-window window)
  (car (*window-data window)))

(define (browser-add! selected)
  (if (pair? (car selected))
    (case (caar selected)
      ((directory) (scmus-find-add! (cons 'base (cdar selected))))
      ((playlist)  (scmus-playlist-load! (cdar selected)))
      ((file)      (scmus-add! (cdar selected))))))

(define (browser-add-selected! window)
  (for-each browser-add! (window-all-selected window)))

(define (directory-activate! window dir)
  (set-window! 'browser (make-browser-window window (scmus-lsinfo dir)))
  (register-event! 'browser-data-changed))

(define (playlist-activate! window playlist)
  (set-window! 'browser (make-browser-window window (scmus-list-playlist playlist)))
  (register-event! 'browser-data-changed))

(define (file-activate! window file)
  (set-window! 'browser (make-browser-window window file))
  (register-event! 'browser-data-changed))

(define (browser-activate! window)
  (let ((selected (window-selected window)))
    (if (pair? (car selected))
      (case (caar selected)
        ((directory) (directory-activate! window (cdar selected)))
        ((playlist) (playlist-activate! window (cdar selected)))
        ((file) (file-activate! window selected))))))

(define (browser-deactivate! window)
  (when (browser-prev-window window)
    (set-window! 'browser (browser-prev-window window))
    (register-event! 'browser-data-changed)))

(define (browser-match row query)
  (if (pair? (car row))
    (case (caar row)
      ((directory playlist) (string-contains-ci (cdar row) query))
      ((file) (track-match row query))
      (else #f))
    #f))

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

(define (make-browser-window prev-win data)
  (make-window (cons prev-win data)
               browser-window-data
               (lambda (w) (register-event! 'browser-changed))
               browser-activate!
               browser-deactivate!
               browser-match))

(define (make-browser-view)
  (make-view (make-browser-window #f (scmus-lsinfo "/"))
             "Browser"
             browser-window-print-row))

(define (update-browser!)
  (set-window! 'browser (make-browser-window #f (scmus-lsinfo "/")))
  (register-event! 'browser-data-changed))
