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

(module (scmus track)
    (seconds->string
     sort-metadata
     track-file
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
  (import (scmus base))

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
    (if (or (string-contains-ci (track-title track) query)
            (string-contains-ci (track-album track) query)
            (string-contains-ci (track-artist track) query)
            (string-contains-ci (track-albumartist track) query))
      #t
      #f))

  (: seconds->string (fixnum -> string))
  (define (seconds->string total-seconds)
    (assert (>= total-seconds 0) "seconds->string" total-seconds)
    (let* ((total-minutes (quotient total-seconds 60))
           (seconds (modulo total-seconds 60))
           (minutes (modulo total-minutes 60))
           (hours (quotient total-minutes 60)))
      (string-append (if (= hours 0)
                       ""
                       (format "~a:" hours))
                     (if (< minutes 10)
                       (format "0~a:" minutes)
                       (format "~a:" minutes))
                     (if (< seconds 10)
                       (format "0~a" seconds)
                       (number->string seconds)))))

  (define metadata-display-names
    '((title                     . "Title")
      (artist                    . "Artist")
      (artistsort                . "Artist (Sort)")
      (albumartist               . "Album Artist")
      (albumartistsort           . "Albumartist (Sort)")
      (album                     . "Album")
      (date                      . "Date")
      (track                     . "Track Number")
      (disc                      . "Disc Number")
      (genre                     . "Genre")
      (composer                  .  "Composer")
      (duration                  . "Duration")
      (last-modified             . "Last Modified")
      (file                      . "File")
      (musicbrainz_artistid      . "Musicbrainz Artist ID")
      (musicbrainz_albumid       . "Musicbrainz Album ID")
      (musicbrainz_albumartistid . "Musicbrainz Albumartist ID")
      (musicbrainz_trackid       . "Musicbrainz Track ID")))

  (define (metadata-name key)
    (let ((name (alist-ref key metadata-display-names)))
      (if name name (symbol->string key))))

  (: sort-metadata ((list-of (pair symbol *)) -> (list-of (pair string *))))
  (define (sort-metadata metadata)
    ;; returns the position of key in alist, or #f.
    (define (alist-ordinal key alist)
      (let loop ((i 0) (rest alist))
        (if (null? rest)
          #f
          (if (eqv? key (caar rest))
            i
            (loop (+ i 1) (cdr rest))))))

    ;; Returns true if (car a) is before (car b) in model (an alist)
    (define (alist-compare a b model)
      (let ((ord-a (alist-ordinal (car a) model))
            (ord-b (alist-ordinal (car b) model)))
        (cond
          ((and (not ord-a) (not ord-b))
             (string<? (symbol->string (car a))
                       (symbol->string (car b))))
          ((not ord-a) #f)
          ((not ord-b) #t)
          (else (< ord-a ord-b)))))

    ;; We're doing two things here.  First, we sort the metadata using
    ;; metadata-display-names as a reference.  Then we convert the keys to
    ;; strings, using metadata-display-names to get the name strings.
    (map (lambda (x)
           (cons (metadata-name (car x))
                 (cdr x)))
         (sort (alist-delete 'time metadata)
               (lambda (a b)
                 (alist-compare a b metadata-display-names))))))
