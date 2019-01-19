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

(declare (hide))

(import coops-utils
        drewt.ncurses
        scmus.base
        scmus.client
        scmus.command-line
        scmus.event
        scmus.format
        scmus.option
        scmus.track 
        scmus.tui
        scmus.view
        scmus.widgets)

(let ((invalidate (lambda (_) (widget-invalidate library-widget))))
  (add-option-listener 'format-separator        invalidate)
  (add-option-listener 'format-library-playlist invalidate)
  (add-option-listener 'format-library-artist   invalidate)
  (add-option-listener 'format-library-album    invalidate)
  (add-option-listener 'format-library-file     invalidate)
  (add-option-listener 'format-library-metadata invalidate))

(define-class <library-window> (<window>))

(define-method (widget-activate (window <library-window>))
  (define (playlist-activate! playlist)
    (let ((tracks (scmus-list-playlist (cdar playlist))))
      (widget-stack-push! (widget-parent window)
        (make-library-window (map (lambda (track)
                                    (make-window-row track 'file 'format-library-file))
                                  tracks)))))
  (define (artist-activate! artist)
    (let ((albums (apply scmus-list-tags 'album artist)))
      (widget-stack-push! (widget-parent window)
        (make-library-window (map (lambda (album)
                                    (make-window-row (cons album artist)
                                                     'album
                                                     'format-library-album))
                                   albums)))))
  (define (album-activate! album)
    (let ((tracks (apply scmus-find album)))
      (widget-stack-push! (widget-parent window)
        (make-library-window (map (lambda (track)
                                    (make-window-row track 'file 'format-library-file))
                                  tracks)))))
  (define (file-activate! track)
    (widget-stack-push! (widget-parent window)
      (make-library-window (map (lambda (metadata)
                                  (make-window-row (list (cons 'tag (car metadata))
                                                         (cons 'value (cdr metadata)))
                                                   'metadata
                                                   'format-library-metadata))
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

(define-method (widget-remove (window <library-window>))
  (let ((selected (map (lambda (selected)
                         (cdar (window-row-data selected)))
                       (filter (lambda (selected)
                                 (eqv? (window-row-type selected)
                                       'playlist))
                               (window-selected window)))))
    (unless (null? selected)
      (command-line-confirm (if (null? (cdr selected))
                              (format "Delete playlist ~s?" (car selected))
                              (format "Delete (~s) playlists?" (length selected)))
        (lambda ()
          (for-each scmus-playlist-rm! selected)
          (widget-clear-marked window)
          (signal-event/global 'db-changed))))))

(define-method (widget-add (window <library-window>) dst)
  (for-each (lambda (selected)
              (when (instance-of? selected <window-row>)
                (case (window-row-type selected)
                  ((playlist)     (add-playlist dst (cdar (window-row-data selected))))
                  ((artist album) (add/constraints dst (car (window-row-data selected))))
                  ((file)         (add-track dst (window-row-data selected))))))
            (window-selected window))
  (scmus-update-queue!))

(add-listener/global 'db-changed
  (lambda ()
    (set! (list-box-data (widget-last (frame-body (get-view 'library))))
          (append! (cons (make <window-separator> 'text " Playlists" 'cursed CURSED-WIN-TITLE)
                         (map (lambda (x)
                                (make-window-row (list x) 'playlist 'format-library-playlist))
                              (scmus-list-playlists)))
                   (cons (make <window-separator> 'text " Artists" 'cursed CURSED-WIN-TITLE)
                         (map (lambda (x)
                                (make-window-row (list x) 'artist 'format-library-artist))
                              (scmus-list-tags 'albumartist)))))))

(define (make-library-window data)
  (make <library-window>
        'data       data
        'cursed     CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define library-widget (make-widget-stack (make-library-window '())))

(define-view library
  (make-frame 'body   library-widget
              'header (make-text " Library" 'cursed CURSED-WIN-TITLE)))
