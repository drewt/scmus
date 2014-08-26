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

(require-extension srfi-1 srfi-13)

(declare (unit library-view)
         (uses ncurses scmus-client ui-curses window)
         (export library-add-selected! make-library-view update-library!))

(define (library-window-data window)
  (car (*window-data window)))

(define (library-prev-window window)
  (cdr (*window-data window)))

(define (library-changed! w)
  (register-event! 'library-changed))

(define (list-of type lst)
  (map (lambda (x) (cons type x)) lst))

(define (library-add! selected)
  (case (car selected)
    ((playlist) (scmus-playlist-load! (cdr selected)))
    ((artist album) (scmus-search-songs #t #t selected))
    ((track) (scmus-add! (track-file (cdr selected)) #f))))

(define (library-add-selected! window)
  (for-each library-add! (window-all-selected window)))

(define (library-deactivate! window)
  (when (library-prev-window window)
    (set-window! 'library (library-prev-window window))
    (register-event! 'library-data-changed)))

(define (match-function fn)
  (lambda (e q) (fn (cdr e) q)))

(define (make-meta-window prev-win metadata)
  (make-window (cons (list-of 'metadata metadata) prev-win)
               library-window-data
               library-changed!
               void
               library-deactivate!
               (lambda (e q) #f)))

(define (track-activate! window)
  (set-window! 'library (make-meta-window window (cdr (window-selected window))))
  (register-event! 'library-data-changed))

(define (make-tracks-window prev-win tracks)
  (make-window (cons (list-of 'track tracks) prev-win)
               library-window-data
               library-changed!
               track-activate!
               library-deactivate!
               (match-function track-match)))

(define (album-activate! window)
  (let* ((album (cdr (window-selected window)))
         (tracks (scmus-search-songs #t #f (cons 'album album))))
    (set-window! 'library (make-tracks-window window tracks))
    (register-event! 'library-data-changed)))

(define (make-albums-window prev-win albums)
  (make-window (cons albums prev-win)
               library-window-data
               library-changed!
               album-activate!
               library-deactivate!
               (match-function string-contains-ci)))

(define (artist-activate! window artist)
  (let ((albums (scmus-list-tags 'album (cons 'artist artist))))
    (set-window! 'library (make-albums-window window albums))
    (register-event! 'library-data-changed)))

(define (playlist-activate! window playlist)
  (let ((tracks (scmus-list-playlist playlist)))
    (set-window! 'library (make-tracks-window window tracks))
    (register-event! 'library-data-changed)))

(define (toplevel-activate! window)
  (let ((selected (window-selected window)))
    (case (car selected)
      ((artist) (artist-activate! window (cdr selected)))
      ((playlist) (playlist-activate! window (cdr selected))))))

(define (toplevel-get-data window)
  (unless (*window-data window)
    (*window-data-set! window (cons (append! (cons '(separator . "Playlists")
                                                   (scmus-list-playlists))
                                             (cons '(separator . "Artists")
                                                   (scmus-list-tags 'artist)))
                                    #f))
    (window-data-len-update! window))
  (library-window-data window))

(define (update-library!)
  (set-window! 'library (make-library-window))
  (register-event! 'library-data-changed))

(define (library-window-print-row window row line-nr cursed)
  (case (car row)
    ((separator playlist artist) (simple-print-line line-nr (cdr row)))
    ((album) (simple-print-line line-nr (cdr row)))
    ((track) (track-print-line line-nr (get-format 'format-library) (cdr row)))
    ((metadata) (alist-print-line window (cdr row) line-nr cursed))))

(define (make-library-window)
  (make-window #f
               toplevel-get-data
               library-changed!
               toplevel-activate!
               void
               (match-function string-contains-ci)))

(define (make-library-view)
  (make-view (make-library-window)
             "Library"
             library-window-print-row))
