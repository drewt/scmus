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

(declare (unit input)
         (uses editable keys ncurses)
         (export current-editable cursor-pos handle-input set-input-mode!))

(define *current-input-mode* 'normal-mode)
(define *current-editable* #f)
(define *editable-pos* #f)

(define (current-editable) *current-editable*)

(define (cursor-pos)
  (cons (car *editable-pos*)
        (min (+ (cdr *editable-pos*)
                (editable-cursor-pos *current-editable*))
             (- (COLS) 1))))

(define (set-input-mode! mode #!optional (arg0 #f) (arg1 #f))
  (assert (symbol? mode) "set-input-mode!" mode)
  (assert (memv mode '(normal-mode edit-mode)) "set-input-mode!" mode)
  (case mode
    ((normal-mode) (enter-normal-mode))
    ((edit-mode)   (assert (editable? arg0) "set-input-mode!" arg0)
                   (assert (pair? arg1) "set-input-mode!" arg1)
                   (assert (and (integer? (car arg1)) (integer? (cdr arg1)))
                           "set-input-mode!" (car arg1) (cdr arg1))
                   (set! *current-editable* arg0)
                   (set! *editable-pos* arg1)
                   (cursor-on)
                   (editable-init arg0)))
  (set! *current-input-mode* mode))

(define (handle-key view key)
  (cond
    ((= key KEY_RESIZE) (redraw-ui))
    (else
      (case *current-input-mode*
        ((normal-mode) (normal-mode-key view key))
        ((edit-mode)   (editable-key *current-editable* key))))))

(define (handle-char view ch)
  (case *current-input-mode*
    ((normal-mode) (normal-mode-char view ch))
    ((edit-mode)   (editable-char *current-editable* ch))))

(define (handle-input view)
  (let-values (((ch rc) (get-char)))
    (cond
      ((= rc KEY_CODE_YES) (handle-key view ch))
      ((= rc ERR) #f)
      (else (handle-char view (integer->char ch))))))
