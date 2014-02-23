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

;;
;; NOTE: this code is pretty inefficient.  It may be worthwhile to
;;       rework cmus's format_print.c and make a wrapper.
;;

(require-extension srfi-1)

(declare (unit format)
         (uses scmus-client)
         (export scmus-format
                 process-format
                 format-string-valid?))

(define (swap pair)
  (cons (cdr pair) (car pair)))

;; Takes a processed format string (see: process-format)
;; and returns a pair of strings, where the car is the
;; left-justified part and the cdr is the right justified
;; part.
(define (scmus-format fmt len track)
  (let ((pair (foldl format-concatenate
                     '("" . "")
                     (map (lambda (x) (format-replace x track)) fmt))))
    (cons (string-truncate (cdr pair) len)
          (string-truncate-left (car pair) len))))

(define (format-concatenate pair e)
  (if (symbol? e)
    (swap pair)
    (cons (string-append (car pair) e) (cdr pair))))

(define (scmus-state->character state)
  (case state
    ((play) ">")
    ((stop) ".")
    ((pause) "|")
    ((unknown) "?")))

(define (format-replace e track)
  (if (symbol? e)
    (case e
      ((artist) (track-artist track))
      ((album) (track-album track))
      ((albumartist) (track-albumartist track))
      ((discnumber) (track-disc track))
      ((tracknumber) (track-track track))
      ((title) (track-title track))
      ((genre) (track-genre track))
      ((comment) (track-comment track))
      ((date) (track-date track))
      ((duration) (seconds->string (track-duration track)))
      ((path) (track-file track)) ; FIXME: need to prepend mpd music dir
      ((filename) (track-file track)) ; FIXME: need to extract filename
      ((align) 'align)
      ((playing) (scmus-state->character (scmus-state)))
      ((current) (scmus-elapsed))
      ((db-playtime) (seconds->string (scmus-db-playtime)))
      ((volume) (number->string (scmus-volume)))
      (else "<FORMAT ERROR>"))
    (string e)))

(define (format-spec->symbol spec)
  (case (car spec)
    ((#\a) 'artist)
    ((#\l) 'album)
    ((#\A) 'albumartist)
    ((#\D) 'discnumber)
    ((#\n) 'tracknumber)
    ((#\t) 'title)
    ((#\g) 'genre)
    ((#\c) 'comment)
    ((#\y) 'date)
    ((#\d) 'duration)
    ((#\f) 'path)
    ((#\F) 'filename)
    ((#\=) 'align)
    ((#\P) 'playing)
    ((#\p) 'current)
    ((#\T) 'db-playtime)
    ((#\v) 'volume)))

;; skips over a format spec in a char list
(define (format-next str)
  (cdr str)) ; TODO: multi-char spec

(define (format-spec-valid? spec)
  (if (null? spec)
    #f
    (case (car spec)
      ((#\a #\A #\l #\D #\n #\t #\g #\c #\y #\d #\f #\F #\~ #\= #\P #\p)
        #t)
      (else ; TODO: multi-char spec
        #f))))

;; this should be called on any user-entered format string
(define (format-string-valid? chars)
  (cond
    ((null? chars) #t)
    ((not (eqv? (car chars) #\~))
      (format-string-valid? (cdr chars)))
    ((format-spec-valid? (cdr chars))
      (format-string-valid? (format-next (cdr chars))))
    (else #f)))

;; replaces format specifiers with symbols.
;; chars is assumed valid.
(define (process-format chars)
  (define (*process-format in out)
    (cond
      ((null? in)
        out)
      ((not (eqv? (car in) #\~))
        (*process-format (cdr in)
                         (cons (car in) out)))
      (else
        (*process-format (format-next (cdr in))
                         (cons (format-spec->symbol (cdr in)) out)))))
  ; if the format string didn't contain "~=", 'align is added at the end
  (let ((processed (*process-format chars '())))
    (reverse (if (member 'align processed)
               processed
               (cons 'align processed)))))
