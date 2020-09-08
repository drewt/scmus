;;
;; Copyright 2014-2020 Drew Thoreson
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

(import (scmus base)
        (scmus client)
        (scmus event)
        (scmus option)
        (scmus status)
        (scmus track)
        (scmus tui)
        (scmus view)
        (scmus widgets))

(define-class <playlist-window> (<window>))

(define-method (widget-add (window <playlist-window>) dst)
  (for-each (lambda (selected)
              (unless (eqv? dst 'playlist)
                (add-track dst (window-row-data selected))))
            (window-selected window)))

(define-method (widget-remove (window <playlist-window>))
  (for-each (lambda (x) (scmus-playlist-delete! (current-playlist-name) x))
            (sort (window-marked window) >))
  (widget-clear-marked window)
  (scmus-playlist-edit (current-playlist-name)))

(define-method (widget-paste (window <playlist-window>) before?)
  (let loop ((marked (sort (*window-marked window) <))
             (pos    (- (list-box-sel-pos window)
                        (if before? 1 0))))
    (unless (null? marked)
      (if (< (car marked) pos)
        (begin
          (scmus-playlist-move! (current-playlist-name) (car marked) pos)
          (loop (map (lambda (x) (if (< x pos) (- x 1) x))
                     (cdr marked))
                pos))
        (begin
          (scmus-playlist-move! (current-playlist-name) (car marked) (+ 1 pos))
          (loop (cdr marked) (+ 1 pos))))))
  (widget-clear-marked window)
  (scmus-playlist-edit (current-playlist-name)))

(define-method (widget-clear (window <playlist-window>))
  (scmus-playlist-close))

(define playlist-widget
  (make <playlist-window>
        'data '()
        'cursed CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define playlist-header
  (make-format-text " Playlist Editor - ~{name}"
                    '((name . "(no playlist loaded)"))
                    'cursed CURSED-WIN-TITLE))

(define-view playlist
  (make-frame
    'body   playlist-widget
    'header playlist-header))

;; Update window when (current-playlist) changes.
(add-listener/global 'playlist-changed
  (lambda ()
    (set! (list-box-data playlist-widget)
      (map (lambda (track)
             (make-window-row track 'file 'format-playlist))
           (current-playlist)))
    (set! (format-text-data playlist-header)
      `((name . ,(or (current-playlist-name)
                     "(no playlist loaded)"))))))

(add-option-listener 'format-playlist
  (lambda (_) (widget-invalidate playlist-widget)))
