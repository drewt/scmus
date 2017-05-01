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
         (export update-library!))

(import ncurses)

(define (tag-data data)
  (map (lambda (x) (cons (car x) (list x))) data))

(: library-add! (pair -> undefined))
(define (library-add! selected)
  (case (car selected)
    ((playlist) (scmus-playlist-load! (cdadr selected)))
    ((artist album) (scmus-search-songs #t #t (cadr selected)))
    ((file) (scmus-add! (track-file (cdr selected))))))

(: library-add-selected! (window -> undefined))
(define (library-add-selected! window)
  (for-each library-add! (window-all-selected window))
  (scmus-update-queue!))

(: update-library! thunk)
(define (update-library!)
  (let ((window (get-window 'library)))
    (let loop ()
      (when (window-stack-peek window)
        (window-stack-pop! window)
        (loop)))
    (set! (*window-data window) #f)
    (library-get-data window)
    (void)))

(: library-format (symbol -> format-spec))
(define (library-format tag)
  (case tag
    ((separator) (get-format 'format-separator))
    ((playlist)  (get-format 'format-library-playlist))
    ((artist)    (get-format 'format-library-artist))
    ((album)     (get-format 'format-library-album))
    ((file)      (get-format 'format-library-file))
    ((metadata)  (get-format 'format-library-metadata))))

(: library-get-data (window -> list))
(define (library-get-data window)
  (unless (*window-data window)
    (set! (*window-data window) (append! (cons '(separator . ((text . "Playlists")))
                                               (tag-data (scmus-list-playlists)))
                                         (cons '(separator . ((text . "Artists")))
                                               (tag-data (scmus-list-tags 'artist))))))
  (*window-data window))

(: library-activate! (window -> undefined))
(define (library-activate! window)
  (define (activate-function type)
    (case type
      ((playlist) playlist-activate!)
      ((artist)   artist-activate!)
      ((album)    album-activate!)
      ((file)     file-activate!)
      (else       void)))
  (let ((selected (window-selected window)))
    ((activate-function (car selected)) window (cdr selected))))

(: playlist-activate! (window list -> undefined))
(define (playlist-activate! window playlist)
  (let ((tracks (scmus-list-playlist (cdar playlist))))
    (window-stack-push! window (list-of 'file tracks) library-get-data)))

(: artist-activate! (window list -> undefined))
(define (artist-activate! window artist)
  (let ((albums (scmus-list-tags 'album (cons 'artist (cdar artist)))))
    (window-stack-push! window (tag-data albums) library-get-data)))

(: album-activate! (window list -> undefined))
(define (album-activate! window album)
  (let ((tracks (scmus-search-songs #t #f (cons 'album (cdar album)))))
    (window-stack-push! window (list-of 'file tracks) library-get-data)))

(: file-activate! (window track -> undefined))
(define (file-activate! window track)
  (define (format-metadata metadata)
    (map (lambda (x)
           (cons 'metadata
                 (list (cons 'tag   (car x))
                       (cons 'value (cdr x)))))
         metadata))
  (window-stack-push! window (format-metadata (sort-metadata track)) library-get-data))

(: library-deactivate! (window -> undefined))
(define (library-deactivate! window)
  (when (window-stack-peek window)
    (window-stack-pop! window)))

(: library-match (* string -> boolean))
(define (library-match row query)
  (case (car row)
    ((playlist artist album) (substring-match (cdadr row) query))
    ((track)                 (track-match (cdr row) query))
    ((metadata)              (or (substring-match (cdadr row) query)
                                 (substring-match (format "~a" (cdaddr row)) query)))
    (else                    #f)))

(define-view library
  (make-view (make-stack-window 'data #f
                                'data-thunk library-get-data
                                'activate   library-activate!
                                'deactivate library-deactivate!
                                'match      library-match
                                'add        library-add-selected!
                                'format     library-format)
             " Library"))
