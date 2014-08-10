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

(declare (unit *library)
         (uses scmus-client window ui-curses)
         (export *current-library-window* make-library-window
                 lib-selected-artist lib-add-selected!))

(define *current-library-window* 'artist)
(define *library-windows*
  '((artist . '())
    (album . '())
    (track . '())))

(define (lib-selected-artist)
  (window-selected (alist-ref 'artist *library-windows*)))

;; This is the "proper" function to get all constraints before searching.
;; This isn't used because scmus isn't smart about compilations (yet).
(define (lib-all-constraints)
  (let* ((artists-win (alist-ref 'artist *library-windows*))
         (albums-win (alist-ref 'album *library-windows*))
         (artists (window-data artists-win))
         (albums (window-data albums-win)))
    (cond
      ((null? artists) (list (cons 'artist "")))
      ((null? albums)  (list (cons 'artist (window-selected artists-win))))
      (else            (list (cons 'artist (window-selected artists-win))
                             (cons 'album  (window-selected albums-win)))))))

;; This is used to get just the most relevant constraint before searching,
;; which is Wrong but makes it possible to access compilations conveniently.
(define (lib-selected-constraint selected)
  (list (cons *current-library-window* selected)))

(define (lib-activate-fn tag next-win-name next-list-gen)
  (lambda (window)
    (let* ((next-win (alist-ref next-win-name *library-windows*))
           (constraint (cons tag (window-selected window)))
           (next-list (next-list-gen constraint)))
      (*window-data-set! next-win next-list)
      (window-clear-marked! next-win)
      (set-window! 'library next-win)
      (set! *current-library-window* next-win-name)
      (register-event! 'library-data-changed))))

(define lib-artist-activate!
  (lib-activate-fn 'artist 'album
    (lambda (constraint) (scmus-search-by-tag 'album constraint))))

(define lib-album-activate!
  (lib-activate-fn 'album 'track
    (lambda (constraint) (scmus-search-songs #t #f constraint))))

(define (lib-track-activate! window)
  (void))

(define (lib-deactivate-fn prev-win-name)
  (lambda (window)
    (let ((prev-win (alist-ref prev-win-name *library-windows*)))
      (*window-data-set! window '())
      (window-top-pos-set! window 0)
      (window-sel-pos-set! window 0)
      (window-match-pos-set! window 0)
      (window-clear-marked! prev-win)
      (set-window! 'library prev-win)
      (set! *current-library-window* prev-win-name)
      (register-event! 'library-data-changed))))

(define lib-album-deactivate! (lib-deactivate-fn 'artist))
(define lib-track-deactivate! (lib-deactivate-fn 'album))

(define (lib-add-selected! window pos)
  (let ((selected (window-all-selected window)))
    (if (eqv? *current-library-window* 'track)
      (let loop ((selected selected) (pos pos))
        (unless (null? selected)
          (scmus-add! (track-file (car selected)) pos)
          (loop (cdr selected) (if pos (+ pos 1) #f))))
      (let loop ((selected selected))
        (unless (null? selected)
          (apply scmus-search-songs #t #t
                 (lib-selected-constraint (car selected)))
          (loop (cdr selected)))))))

(define (make-library-window)
  (define (lib-changed! w) (register-event! 'library-changed))
  (alist-update! 'artist
                 (make-window #f
                              (lambda (w) *artists*)
                              lib-changed!
                              lib-artist-activate!
                              void
                              string-contains-ci)
                 *library-windows*)
  (alist-update! 'album
                 (make-window '()
                              *window-data
                              lib-changed!
                              lib-album-activate!
                              lib-album-deactivate!
                              string-contains-ci)
                 *library-windows*)
  (alist-update! 'track
                 (make-window '()
                              *window-data
                              lib-changed!
                              lib-track-activate!
                              lib-track-deactivate!
                              track-match)
                 *library-windows*)
  (alist-ref 'artist *library-windows*))
