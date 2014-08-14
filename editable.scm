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

(require-extension srfi-1 ncurses)

;;
;; Simple text representation suitable for editing.  Text is stored as a
;; reversed list of characters, and can be converted to a string on demand.
;;
(declare (unit editable))

(define-record-type editable
  (*make-editable char-handler key-handler init char-list cursor-pos text-length)
  editable?
  (char-handler editable-char-handler editable-char-handler-set!)
  (key-handler editable-key-handler editable-key-handler-set!)
  (init *editable-init editable-init-set!)
  (char-list editable-list editable-set-list!)
  (cursor-pos editable-pos editable-set-pos!)
  (text-length editable-length editable-set-length!))

(define (editable-default-char-handler editable ch)
  (case ch
    ((#\backspace) (editable-backspace! editable))
    ((#\x4)        (editable-delete-char! editable))
    (else          (editable-insert! editable ch))))

(define (editable-default-key-handler editable key)
  (key-case key
    ((KEY_LEFT)      (editable-move-left! editable))
    ((KEY_RIGHT)     (editable-move-right! editable))
    ((KEY_BACKSPACE) (editable-backspace! editable))
    ((KEY_HOME)      (editable-move-home! editable))
    ((KEY_END)       (editable-move-end! editable))
    ((KEY_DC)        (editable-delete-char! editable))))

(define (make-editable text #!optional
                       (char-handler editable-default-char-handler)
                       (key-handler editable-default-key-handler)
                       (init editable-move-end!))
  (*make-editable char-handler
                  key-handler
                  init
                  (reverse (string->list text))
                  0
                  (string-length text)))

(define (simple-char-handler activate leave changed)
  (lambda (editable ch)
    (case ch
      ((#\newline)
        (activate editable)
        (leave editable))
      ((#\esc)
        (leave editable))
      (else
        (editable-default-char-handler editable ch)))
    (changed)))

(define (simple-key-handler activate leave changed)
  (lambda (editable key)
    (key-case key
      ((KEY_ENTER)
        (activate editable)
        (leave editable))
      (else (editable-default-key-handler editable key)))
    (changed)))

(define (make-simple-editable activate leave changed #!optional (text ""))
  (*make-editable (simple-char-handler activate leave changed)
                  (simple-key-handler activate leave changed)
                  editable-move-end!
                  (reverse (string->list text))
                  0
                  (string-length text)))

(define (editable-text editable)
  (list->string (reverse (editable-list editable))))

(define (editable-insert! editable ch)
  (define (list-insert l pos elm)
    (append (take l pos)
            (cons elm (drop l pos))))
  (editable-set-list! editable
                      (list-insert (editable-list editable)
                                   (editable-pos editable)
                                   ch))
  (editable-set-length! editable (+ (editable-length editable) 1)))

(define (editable-backspace! editable)
  (define (list-delete l pos)
    (append (take l pos)
            (drop l (+ pos 1))))
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

(define (editable-text-set! editable text)
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

(define (editable-cursor-pos editable)
  (let loop ((chars (reverse (editable-list editable)))
             (pos (- (editable-length editable)
                     (editable-pos editable))))
    (if (or (= 0 pos) (null? chars))
      0
      (+ (char-width (car chars))
         (loop (cdr chars) (- pos 1))))))

(define (editable-char editable ch)
  (assert (editable? editable))
  (assert (char? ch))
  ((editable-char-handler editable) editable ch))

(define (editable-key editable key)
  (assert (editable? editable))
  (assert (integer? key))
  ((editable-key-handler editable) editable key))

(define (editable-init editable)
  ((*editable-init editable) editable))
