;;
;; Copyright 2014-2017 Drew Thoreson
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

(declare (unit track))

(module track (track-file
               track-last-modified
               *track-duration
               track-title
               track-artist
               track-album
               track-albumartist
               track-genre
               track-date
               track-track
               track-disc
               track-name
               track-composer
               track-performer
               track-start
               track-end
               track-pos
               track-id
               track-prio
               track-duration
               track-meta
               track=
               track-match)
  (import scmus-base)
 
  (define-syntax track-selector
    (syntax-rules ()
      ((track-selector name sym)
        (track-selector name sym ""))
      ((track-selector name sym default)
        (define (name song)
          (assert (list? song) "" song)
          (let ((e (alist-ref sym song)))
           (if e e default))))))

  (track-selector track-file 'file)
  (track-selector track-last-modified 'last-modified 0)
  (track-selector *track-duration 'time '(0))
  (track-selector track-title 'title)
  (track-selector track-artist 'artist)
  (track-selector track-album 'album)
  (track-selector track-albumartist 'albumartist)
  (track-selector track-genre 'genre)
  (track-selector track-date 'date)
  (track-selector track-track 'track)
  (track-selector track-disc 'disc 1)
  (track-selector track-name 'name)
  (track-selector track-composer 'composer)
  (track-selector track-performer 'performer)
  (track-selector track-start 'start 0)
  (track-selector track-end 'end 0)
  (track-selector track-pos 'pos -1)
  (track-selector track-id 'id -1)
  (track-selector track-prio 'prio 0)

  (: track-duration (track -> number))
  (define (track-duration track)
    (car (*track-duration track)))

  (: track-meta (track symbol #!optional * -> *))
  (define (track-meta track meta #!optional (default ""))
    (let ((pair (assoc meta track)))
      (if pair
        (cdr pair)
        default)))

  (: track= (track track -> boolean))
  (define (track= a b)
    (string=? (track-file a) (track-file b)))

  (: track-match (track string -> boolean))
  (define (track-match track query)
    (if (or (substring-match (track-title track) query)
            (substring-match (track-album track) query)
            (substring-match (track-artist track) query)
            (substring-match (track-albumartist track) query))
      #t
      #f)))
