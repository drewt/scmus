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

(declare (unit library-view)
         (uses event ncurses option scmus-client ui-lib view window)
         (export library-add-selected! make-library-view update-library!))

(: library-window-data (window -> list))
(define (library-window-data window)
  (car (*window-data window)))

(: library-prev-window (window -> window))
(define (library-prev-window window)
  (cdr (*window-data window)))

(: library-changed! (window -> undefined))
(define (library-changed! w)
  (register-event! 'library-changed))

(: list-of (symbol list -> (list-of (pair symbol *))))
(define (list-of type lst)
  (map (lambda (x) (cons type x)) lst))

(: library-add! (pair -> undefined))
(define (library-add! selected)
  (case (car selected)
    ((playlist) (scmus-playlist-load! (cdr selected)))
    ((artist album) (scmus-search-songs #t #t selected))
    ((track) (scmus-add! (track-file (cdr selected))))))

(: library-add-selected! (window -> undefined))
(define (library-add-selected! window)
  (for-each library-add! (window-all-selected window)))

(: library-deactivate! (window -> undefined))
(define (library-deactivate! window)
  (when (library-prev-window window)
    (set-window! 'library (library-prev-window window))
    (register-event! 'library-data-changed)))

(: match-function ((* string -> *) -> (pair string -> boolean)))
(define (match-function fn)
  (lambda (e q) (fn (cdr e) q)))

(: make-meta-window (window list -> window))
(define (make-meta-window prev-win metadata)
  (make-window data:       (cons (list-of 'metadata metadata) prev-win)
               get-data:   library-window-data
               changed:    library-changed!
               deactivate: library-deactivate!))

(: track-activate! (window -> undefined))
(define (track-activate! window)
  (set-window! 'library (make-meta-window window (cdr (window-selected window))))
  (register-event! 'library-data-changed))

(: make-tracks-window (window (list-of track) -> window))
(define (make-tracks-window prev-win tracks)
  (make-window data:       (cons (list-of 'track tracks) prev-win)
               get-data:   library-window-data
               changed:    library-changed!
               activate:   track-activate!
               deactivate: library-deactivate!
               match:      (match-function track-match)))

(: album-activate! (window -> undefined))
(define (album-activate! window)
  (let* ((album (cdr (window-selected window)))
         (tracks (scmus-search-songs #t #f (cons 'album album))))
    (set-window! 'library (make-tracks-window window tracks))
    (register-event! 'library-data-changed)))

(: make-albums-window (window list -> window))
(define (make-albums-window prev-win albums)
  (make-window data:       (cons albums prev-win)
               get-data:   library-window-data
               changed:    library-changed!
               activate:   album-activate!
               deactivate: library-deactivate!
               match:      (match-function substring-match)))

(: artist-activate! (window string -> undefined))
(define (artist-activate! window artist)
  (let ((albums (scmus-list-tags 'album (cons 'artist artist))))
    (set-window! 'library (make-albums-window window albums))
    (register-event! 'library-data-changed)))

(: playlist-activate! (window string -> undefined))
(define (playlist-activate! window playlist)
  (let ((tracks (scmus-list-playlist playlist)))
    (set-window! 'library (make-tracks-window window tracks))
    (register-event! 'library-data-changed)))

(: toplevel-activate! (window -> undefined))
(define (toplevel-activate! window)
  (let ((selected (window-selected window)))
    (case (car selected)
      ((artist) (artist-activate! window (cdr selected)))
      ((playlist) (playlist-activate! window (cdr selected))))))

(: toplevel-get-data (window -> list))
(define (toplevel-get-data window)
  (when (null? (*window-data window))
    (*window-data-set! window (cons (append! (cons '(separator . "Playlists")
                                                   (scmus-list-playlists))
                                             (cons '(separator . "Artists")
                                                   (scmus-list-tags 'artist)))
                                    #f))
    (window-data-len-update! window))
  (library-window-data window))

(: update-library! thunk)
(define (update-library!)
  (set-window! 'library (make-library-window))
  (register-event! 'library-data-changed))

(: library-window-print-row (window pair fixnum fixnum -> undefined))
(define (library-window-print-row window row line-nr cursed)
  (case (car row)
    ((separator playlist artist) (simple-print-line line-nr (cdr row)))
    ((album) (simple-print-line line-nr (cdr row)))
    ((track) (track-print-line line-nr (get-format 'format-library) (cdr row) cursed))
    ((metadata) (alist-print-line window (cdr row) line-nr cursed))))

(: make-library-window (-> window))
(define (make-library-window)
  (make-window get-data: toplevel-get-data
               changed:  library-changed!
               activate: toplevel-activate!
               match:    (match-function substring-match)))

(define-view library
  (make-view (make-library-window)
             "Library"
             library-window-print-row
             add: library-add-selected!))

(define-event (library-changed)
  (update-view! 'library))

(define-event (library-data-changed)
  (window-data-len-update! (get-window 'library))
  (update-view! 'library))
