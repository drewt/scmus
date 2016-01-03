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

(: update-library! thunk)
(define (update-library!)
  (let ((window (get-window 'library)))
    (let loop ()
      (when (window-stack-peek window)
        (window-stack-pop! window)
        (loop)))
    (set! (*window-data window) #f)
    (library-get-data window))
  (register-event! 'library-data-changed))

(: library-print-line (window pair fixnum -> string))
(define (library-print-line window row nr-cols)
  (case (car row)
    ((separator playlist artist album)
      (format "~a" (cdr row)))
    ((track)
      ; FIXME: calling get-format for every line...
      (scmus-format (get-format 'format-library) nr-cols (cdr row)))
    ((metadata)
      (alist-print-line window (cdr row) nr-cols))))

(: library-get-data (window -> list))
(define (library-get-data window)
  (unless (*window-data window)
    (set! (*window-data window) (append! (cons '(separator . "Playlists")
                                               (scmus-list-playlists))
                                         (cons '(separator . "Artists")
                                               (scmus-list-tags 'artist))))
    (window-data-len-update! window))
  (*window-data window))

(: library-activate! (window -> undefined))
(define (library-activate! window)
  (define (activate-function type)
    (case type
      ((playlist) playlist-activate!)
      ((artist)   artist-activate!)
      ((album)    album-activate!)
      ((track)    track-activate!)
      (else       void)))
  (let ((selected (window-selected window)))
    ((activate-function (car selected)) window (cdr selected))))

(: playlist-activate! (window string -> undefined))
(define (playlist-activate! window playlist)
  (let ((tracks (scmus-list-playlist playlist)))
    (window-stack-push! window (list-of 'track tracks) library-get-data)
    (register-event! 'library-data-changed)))

(: artist-activate! (window string -> undefined))
(define (artist-activate! window artist)
  (let ((albums (scmus-list-tags 'album (cons 'artist artist))))
    (window-stack-push! window albums library-get-data)
    (register-event! 'library-data-changed)))

(: album-activate! (window string -> undefined))
(define (album-activate! window album)
  (let ((tracks (scmus-search-songs #t #f (cons 'album album))))
    (window-stack-push! window (list-of 'track tracks) library-get-data)
    (register-event! 'library-data-changed)))

(: track-activate! (window track -> undefined))
(define (track-activate! window track)
  (window-stack-push! window (list-of 'metadata track) library-get-data)
  (register-event! 'library-data-changed))

(: library-deactivate! (window -> undefined))
(define (library-deactivate! window)
  (when (window-stack-peek window)
    (window-stack-pop! window)
    (register-event! 'library-data-changed)))

(: library-match (* string -> boolean))
(define (library-match row query)
  (case (car row)
    ((playlist artist album) (substring-match (cdr row) query))
    ((track)                 (track-match (cdr row) query))
    ((metadata)              (or (substring-match (symbol->string (cadr row)) query)
                                 (substring-match (cddr row) query)))
    (else                    #f)))

(define-view library
  (make-view (make-stack-window 'data #f
                                'data-thunk library-get-data
                                'activate   library-activate!
                                'deactivate library-deactivate!
                                'changed    (lambda (w) (register-event! 'view-changed 'library))
                                'match      library-match
                                'print-line library-print-line)
             " Library"
             add: library-add-selected!))

(define-event-handler (library-data-changed) ()
  (window-data-len-update! (get-window 'library))
  (update-view! 'library))
