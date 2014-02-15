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
;; Simple text representation suitable for editing.  Text is stored as a
;; reversed list of characters, and can be converted to a string on demand.
;;
(declare (unit editable))

(require-extension srfi-1)

(define-record-type editable
  (make-editable char-list cursor-pos text-length)
  editable?
  (char-list editable-list editable-set-list!)
  (cursor-pos editable-pos editable-set-pos!)
  (text-length editable-length editable-set-length!))

(define (list-insert l pos elm)
  (append (take l pos)
          (cons elm (drop l pos))))

(define (list-delete l pos)
  (append (take l pos)
          (drop l (+ pos 1))))
 
(define (make-empty-editable)
  (make-editable '() 0 0))

(define (make-editable-with-text text)
  (make-editable (reverse (string->list text))
                 0
                 (string-length text)))

(define (editable-text editable)
  (list->string (reverse (editable-list editable))))

(define (editable-insert! editable ch)
  (editable-set-list! editable
                      (list-insert (editable-list editable)
                                   (editable-pos editable)
                                   ch))
  (editable-set-length! editable (+ (editable-length editable) 1)))

(define (editable-backspace! editable)
  (when (< (editable-pos editable)
           (editable-length editable))
    (editable-set-list! editable
                        (list-delete (editable-list editable)
                                     (editable-pos editable)))
    (editable-set-length! editable
                          (- (editable-length editable) 1))))

(define (editable-delete-char! editable)
  (when (> (editable-pos editable) 0)
    (editable-move-right! editable)
    (editable-backspace! editable)))

(define (editable-set-text! editable text)
  (editable-set-list! editable
                      (reverse (string->list text)))
  (editable-set-length! editable
                        (string-length text))
  (editable-set-pos! editable 0))

(define (editable-clear! editable)
  (editable-set-list! editable '())
  (editable-set-length! editable 0)
  (editable-set-pos! editable 0))
 
(define (editable-move-left! editable)
  (if (< (editable-pos editable)
         (editable-length editable))
    (editable-set-pos! editable
                       (+ (editable-pos editable) 1))))

(define (editable-move-right! editable)
  (if (> (editable-pos editable) 0)
    (editable-set-pos! editable
                       (- (editable-pos editable) 1))))

(define (editable-move-home! editable)
  (editable-set-pos! editable
                     (editable-length editable)))

(define (editable-move-end! editable)
  (editable-set-pos! editable 0))

