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

(declare (export))

(import drewt.ncurses)
(import coops-utils)
(import scmus.base
        scmus.client
        scmus.event
        scmus.format
        scmus.track 
        scmus.tui
        scmus.widgets)

(: library-format (symbol -> format-spec))
(define (library-format row)
  (case (window-row-type row)
    ((separator) (get-format 'format-separator))
    ((playlist)  (get-format 'format-library-playlist))
    ((artist)    (get-format 'format-library-artist))
    ((album)     (get-format 'format-library-album))
    ((file)      (get-format 'format-library-file))
    ((metadata)  (get-format 'format-library-metadata))))

(define (library-activate! window)
  (define (activate-function type)
    (case type
      ((playlist) playlist-activate!)
      ((artist)   artist-activate!)
      ((album)    album-activate!)
      ((file)     file-activate!)
      (else       void)))
  (let ((selected (window-selected window)))
   (when (instance-of? selected <window-row>)
     ((activate-function (window-row-type selected)) window (window-row-data selected)))))

(define (playlist-activate! window playlist)
  (let ((tracks (scmus-list-playlist (cdar playlist))))
    (widget-stack-push! (widget-parent window)
      (make-library-window (map (lambda (track)
                                  (make-window-row track 'file library-format))
                                tracks)))))

(define (artist-activate! window artist)
  (let ((albums (scmus-list-tags 'album (cons 'artist (cdar artist)))))
    (widget-stack-push! (widget-parent window)
      (make-library-window (map (lambda (album)
                                  (make-window-row (list album) 'album library-format))
                                 albums)))))

(define (album-activate! window album)
  (let ((tracks (scmus-search-songs #t #f (cons 'album (cdar album)))))
    (widget-stack-push! (widget-parent window)
      (make-library-window (map (lambda (track)
                                  (make-window-row track 'file library-format))
                                tracks)))))

(define (file-activate! window track)
  (widget-stack-push! (widget-parent window)
    (make-library-window (map (lambda (metadata)
                                (make-window-row (list (cons 'tag (car metadata))
                                                       (cons 'value (cdr metadata)))
                                                 'metadata
                                                 library-format))
                              track))))

(define (library-deactivate! window)
  (let ((window (widget-parent window)))
    (when (widget-stack-peek window)
      (widget-stack-pop! window))))

(define (library-match row query)
  (if (instance-of? row <window-row>)
    (let ((data (window-row-data row)))
      (case (window-row-type row)
        ((playlist artist album) (substring-match (cdar data) query))
        ((file)                  (track-match data query))
        ((metadata)              (or (substring-match (format "~a" (cdar data)) query)
                                     (substring-match (format "~a" (cdadr data)) query)))
        (else #f)))
    #f))

(define (library-add-selected! window)
  (for-each (lambda (selected)
              (when (instance-of? selected <window-row>)
                (case (window-row-type selected)
                  ((playlist)     (scmus-playlist-load! (cdar (window-row-data selected))))
                  ((artist album) (scmus-search-songs #t #t (car (window-row-data selected))))
                  ((file)         (scmus-add! (track-file (window-row-data selected)))))))
            (window-all-selected window))
  (scmus-update-queue!))

(define-event-handler (db-changed) ()
  (set! (window-data (widget-last (frame-body (get-view 'library))))
        (append! (cons (make <window-separator> 'text "Playlists" 'cursed CURSED-WIN-TITLE)
                       (map (lambda (x) (make-window-row (list x) 'playlist library-format))
                            (scmus-list-playlists)))
                 (cons (make <window-separator> 'text "Artists" 'cursed CURSED-WIN-TITLE)
                       (map (lambda (x) (make-window-row (list x) 'artist library-format))
                            (scmus-list-tags 'artist))))))

(define (make-library-window data)
  (make-window 'data       data
               'activate   library-activate!
               'deactivate library-deactivate!
               'match      library-match
               'add        library-add-selected!
               'format     library-format
               'cursed     CURSED-WIN
               'cursed-fn  (win-cursed-fn)))

(define-view library
  (make-frame 'body   (make-widget-stack (make-library-window '()))
              'header (make-text " Library" 'cursed CURSED-WIN-TITLE)))
