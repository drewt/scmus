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

(require-extension ncurses srfi-1 srfi-13)

(declare (unit library-view)
         (uses scmus-client window ui-curses)
         (export library-add-selected! make-library-window))

(define (library-window-data window)
  (car (*window-data window)))

(define (library-prev-window window)
  (cdr (*window-data window)))

(define (library-changed! w)
  (register-event! 'library-changed))

(define (list-of type lst)
  (map (lambda (x) (cons type x)) lst))

(define (libraryr-add! selected)
  (case (car selected)
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
  (make-window (cons (list-of 'album albums) prev-win)
               library-window-data
               library-changed!
               album-activate!
               library-deactivate!
               (match-function string-contains-ci)))

(define (artist-activate! window artist)
  (let ((albums (scmus-search-by-tag 'album (cons 'artist artist))))
    (set-window! 'library (make-albums-window window albums))
    (register-event! 'library-data-changed)))

(define (playlist-activate! window playlist)
  #f
  )

(define (toplevel-activate! window)
  (let ((selected (window-selected window)))
    (case (car selected)
      ((artist) (artist-activate! window (cdr selected)))
      ((playlist) (playlist-activate! window (cdr selected))))))

(define (toplevel-get-data window)
  (unless (*window-data window)
    (let ((playlists (scmus-list-playlists))
          (artists (scmus-search-by-tag 'artist)))
      (*window-data-set! window (cons (append! (cons '(separator . "Playlists")
                                                     (list-of 'playlist playlists))
                                               (cons '(separator . "Artists")
                                                     (list-of 'artist artists)))
                                     #f))))
  (library-window-data window))

(define (make-library-window)
  (make-window #f
               toplevel-get-data
               library-changed!
               toplevel-activate!
               void
               (match-function string-contains-ci)))
