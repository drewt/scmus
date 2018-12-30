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

(import coops-utils
        drewt.ncurses
        scmus.base
        scmus.client
        scmus.event
        scmus.format
        scmus.track 
        scmus.tui
        scmus.view
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

(define-class <library-window> (<window>))

(define-method (widget-activate (window <library-window>))
  (define (playlist-activate! playlist)
    (let ((tracks (scmus-list-playlist (cdar playlist))))
      (widget-stack-push! (widget-parent window)
        (make-library-window (map (lambda (track)
                                    (make-window-row track 'file library-format))
                                  tracks)))))
  (define (artist-activate! artist)
    (let ((albums (scmus-list-tags 'album (cons 'artist (cdar artist)))))
      (widget-stack-push! (widget-parent window)
        (make-library-window (map (lambda (album)
                                    (make-window-row (list album) 'album library-format))
                                   albums)))))
  (define (album-activate! album)
    (let ((tracks (scmus-search-songs #t #f (cons 'album (cdar album)))))
      (widget-stack-push! (widget-parent window)
        (make-library-window (map (lambda (track)
                                    (make-window-row track 'file library-format))
                                  tracks)))))
  (define (file-activate! track)
    (widget-stack-push! (widget-parent window)
      (make-library-window (map (lambda (metadata)
                                  (make-window-row (list (cons 'tag (car metadata))
                                                         (cons 'value (cdr metadata)))
                                                   'metadata
                                                   library-format))
                                (sort-metadata track)))))
  (define (activate-function type)
    (case type
      ((playlist) playlist-activate!)
      ((artist)   artist-activate!)
      ((album)    album-activate!)
      ((file)     file-activate!)
      (else       void)))
  (unless (list-box-empty? window)
    (let ((selected (list-box-selected window)))
      (when (instance-of? selected <window-row>)
        ((activate-function (window-row-type selected)) (window-row-data selected))))))

(define-method (widget-deactivate (window <library-window>))
  (let ((window (widget-parent window)))
    (when (widget-stack-peek window)
      (widget-stack-pop! window))))

(define-method (widget-add (window <library-window>))
  (for-each (lambda (selected)
              (when (instance-of? selected <window-row>)
                (case (window-row-type selected)
                  ((playlist)     (scmus-playlist-load! (cdar (window-row-data selected))))
                  ((artist album) (scmus-search-songs #t #t (car (window-row-data selected))))
                  ((file)         (scmus-add! (track-file (window-row-data selected)))))))
            (window-selected window))
  (scmus-update-queue!))

(add-listener/global 'db-changed
  (lambda ()
    (set! (list-box-data (widget-last (frame-body (get-view 'library))))
          (append! (cons (make <window-separator> 'text " Playlists" 'cursed CURSED-WIN-TITLE)
                         (map (lambda (x) (make-window-row (list x) 'playlist library-format))
                              (scmus-list-playlists)))
                   (cons (make <window-separator> 'text " Artists" 'cursed CURSED-WIN-TITLE)
                         (map (lambda (x) (make-window-row (list x) 'artist library-format))
                              (scmus-list-tags 'artist)))))))

(define (make-library-window data)
  (make <library-window>
        'data       data
        'cursed     CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define-view library
  (make-frame 'body   (make-widget-stack (make-library-window '()))
              'header (make-text " Library" 'cursed CURSED-WIN-TITLE)))
