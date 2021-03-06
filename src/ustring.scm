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

(module (drewt ustring)
    (color-code?
     ch->color-code
     fg-color->char
     bg-color->char
     ustring-width
     char-width
     ustring-take
     ustring-take-right
     ustring-truncate
     ustring-truncate-left
     ustring-pad
     ustring-stretch)
  (import (scmus base))
  (define-constant +unicode-private-base+ #xE000)

  (: color-code? (char -> boolean))
  (define (color-code? ch)
    (let ((u (char->integer ch)))
      (and (>= u +unicode-private-base+)
           (< u (+ 258 256 +unicode-private-base+)))))

  (: ch->color-code (char -> fixnum symbol))
  (define (ch->color-code ch)
    (let ((code (- (char->integer ch) +unicode-private-base+ 2)))
      (if (> code 255)
        (values (- code 256) 'bg)
        (values code 'fg))))

  (: fg-color->char (fixnum -> char))
  (define (fg-color->char color)
    (integer->char (+ color +unicode-private-base+ 2)))

  (: bg-color->char (fixnum -> char))
  (define (bg-color->char color)
    (integer->char (+ color +unicode-private-base+ 258)))

  (: ustring-width (string -> fixnum))
  (define (ustring-width str)
    (string-fold (lambda (c a) (+ a (char-width c))) 0 str))

  (: char-width (char -> fixnum))
  (define (char-width c)
    (let ((u (char->integer c)))
      (cond
        ((< u #x20) 0)
        ((< u #x1100) 1)
        ; Hangul Jamo init. consonants
        ((<= u #x115f) 2)
        ; Zero-width characters
        ((or (= u #x200b) (= u #x200c) (= u #x200d)) 0)
        ; Angle brackets
        ((or (= u #x2329) (= u #x232a)) 2)
        ((< u #x2e80) 1)
        ; CJK ... Yi
        ((< u #x302a) 2)
        ((<= u #x302f) 1)
        ((= u #x303f) 1)
        ((= u #x3099) 1)
        ((= u #x309a) 1)
        ; CJK ... Yi
        ((<= u #xa4cf) 2)
        ; color codes (private area)
        ((and (>= u +unicode-private-base+)
              (< u (+ 256 +unicode-private-base+))) 0)
        ; Hangul Syllables
        ((and (>= u #xac00) (<= u #xd7a3)) 2)
        ; CJK Compatibility Ideographs
        ((and (>= u #xf900) (<= u #xfaff)) 2)
        ; CJK Compatibility Forms
        ((and (>= u #xfe30) (<= u #xfe6f)) 2)
        ; Fullwidth Forms
        ((and (>= u #xff00) (<= u #xff60)) 2)
        ((and (>= u #xffe0) (<= u #xffe6)) 2)
        ; CJK Extra Stuff
        ((and (>= u #x20000) (<= u #x2fffd)) 2)
        ; ?
        ((and (>= u #x30000) (<= u #x3fffd)) 2)
        (else 1))))

  (: ustring-take (string fixnum -> string fixnum))
  (define (ustring-take str width)
    (define (finalize result space)
      (list->string
        (reverse
          (append (make-list space #\space)
                  result))))
    (let loop ((result '()) (rest (string->list str)) (r-width 0))
      (let* ((c (car rest))
             (c-width (char-width c))
             (space (- width r-width)))
        (if (< space c-width)
          (values (finalize result space) (+ r-width space))
          (loop (cons c result) (cdr rest) (+ r-width c-width))))))

  (: ustring-take-right (string fixnum -> string fixnum))
  (define (ustring-take-right str width)
    (let loop ((result '()) (rest (reverse (string->list str))) (r-width 0))
      (let* ((c (car rest))
             (c-width (char-width c)))
        (if (> (+ r-width c-width) width)
          (values (list->string result) r-width)
          (loop (cons c result) (cdr rest) (+ r-width c-width))))))

  (: ustring-truncate (string fixnum -> string fixnum))
  (define (ustring-truncate s len)
    (let ((width (ustring-width s)))
      (if (> width len)
        (ustring-take s len)
        (values s width))))

  (: ustring-truncate-left (string fixnum -> string fixnum))
  (define (ustring-truncate-left s len)
    (let ((width (ustring-width s)))
      (if (> width len)
        (ustring-take-right s len)
        (values s width))))

  (: ustring-pad (string fixnum char #!optional boolean -> string))
  (define (ustring-pad str len c #!optional (right #f))
    (if right
      (string-append str (make-string (- len (ustring-width str)) c))
      (string-append (make-string (- len (ustring-width str)) c) str)))

  (: ustring-stretch (string char fixnum #!optional boolean -> string))
  (define (ustring-stretch str c len #!optional (right #f))
    (let ((width (ustring-width str)))
      (if (> len width)
        (if right
          (string-append str (make-string (- len width) c))
          (string-append (make-string (- len width) c) str))
        (nth-value 0 (ustring-truncate str len))))))
