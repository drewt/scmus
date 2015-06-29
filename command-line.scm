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

(declare (unit command-line)
         (uses editable eval-mode ncurses ui-curses)
         (export command-line-text-set! command-line-mode command-line-text
                 command-line-print-info! command-line-print-error!
                 enter-eval-mode enter-search-mode))

;; history {{{

(define (iter-data iter)
  (caar iter))

(define (iter-next iter)
  (cdr iter))

(define (iter-prev iter)
  (cdar iter))

(define (iter-move-next iter)
  (if (null? (iter-next iter))
    iter
    (iter-next iter)))

(define (iter-move-prev iter)
  (if (null? (iter-prev iter))
    iter
    (iter-prev iter)))

(define (iter-reset iter)
  (if (or (null? iter) (null? (iter-prev iter)))
    iter
    (iter-reset (iter-prev iter))))

(define (iter-add! iter elm)
  (let* ((head (iter-reset iter))
         (new (cons (cons elm '()) head)))
    (unless (null? head)
      (set-cdr! (car head) new))
    new))

(define (iter-pop! iter)
  (let* ((head (iter-reset iter))
         (next (iter-next head)))
    (if (null? next)
      '()
      (begin
        (set-cdr! (car next) '())
        next))))

(define (iter-set! iter elm)
  (set-car! (car iter) elm))

(define (iter-init! iter)
  (iter-add! (iter-reset iter) ""))

(define history
  (let ((eval-history '())
        (search-history '()))
    (getter-with-setter
      (lambda ()
        (case *command-line-mode*
          ((eval)   eval-history)
          ((search) search-history)
          (else     (assert #f "history-getter"))))
      (lambda (x)
        (case *command-line-mode*
          ((eval)   (set! eval-history x))
          ((search) (set! search-history x))
          (else     (assert #f "history-setter")))))))

(define (history-init!)
  (set! (history) (iter-init! (history))))

(define (history-abort!)
  (set! (history) (iter-pop! (history))))

(define (history-next!)
  (set! (history) (iter-move-next (history))))

(define (history-prev!)
  (set! (history) (iter-move-prev (history))))

(define (history-add! elm)
  (if (string=? elm "")
    (history-abort!)
    (iter-set! (iter-reset (history)) elm)))

(define (history-data)
  (iter-data (history)))

(define (history-set! elm)
  (iter-set! (history) elm))

;; history }}}

(define (command-line-print-info! str)
  (case *command-line-mode*
    ((eval search) (void))
    (else (editable-text-set! *command-line* str)
          (set! *command-line-mode* 'info))))

(define (command-line-print-error! str)
  (case *command-line-mode*
    ((eval search) (void))
    (else (editable-text-set! *command-line* str)
          (set! *command-line-mode* 'error))))

(define (command-line-changed!)
  (register-event! 'command-line-changed))

(define (command-line-empty?)
  (= 0 (command-line-length)))

(define (command-line-clear!)
  (editable-clear! *command-line*)
  (command-line-changed!))

(define (command-line-text)
  (editable-text *command-line*))

(define (command-line-text-set! str)
  (assert (string? str) "command-line-text-set!" str)
  (editable-text-set! *command-line* str))

(define (command-line-cursor-pos)
  (+ 1 (editable-cursor-pos *command-line*)))

(define (command-line-length)
  (editable-length *command-line*))

(define (command-line-leave editable)
  (history-add! (editable-text editable))
  (editable-clear! editable)
  (set! *command-line-mode* 'normal)
  (set-input-mode! 'normal-mode))

(define (command-line-commit! editable)
  (let ((cmdline (editable-text editable))
        (mode *command-line-mode*))
    (command-line-leave editable)
    (if (eqv? mode 'eval)
      (let ((r (user-eval-string cmdline)))
        (if (and (not (condition? r)) (get-option 'eval-mode-print))
          (command-line-print-info! (format "~s" r))))
      (win-search! cmdline))))

(define (command-line-char editable ch)
  (case ch
    ((#\newline)
      (command-line-commit! editable))
    ((#\esc)
      (command-line-leave editable))
    ((#\backspace #\delete)
      (if (= 0 (editable-length editable))
        (command-line-leave editable)
        (editable-default-char-handler editable ch)))
    (else
      (editable-default-char-handler editable ch)))
  (command-line-changed!))

(define (command-line-key editable key)
  (key-case key
    ((KEY_ENTER)
      (command-line-commit! editable))
    ((KEY_UP)
      (history-next!)
      (editable-text-set! editable (history-data)))
    ((KEY_DOWN)
      (history-prev!)
      (editable-text-set! editable (history-data)))
    ((KEY_BACKSPACE)
      (if (= 0 (editable-length editable))
        (command-line-leave editable)
        (editable-default-key-handler editable key)))
    (else (editable-default-key-handler editable key)))
  (command-line-changed!))

(define *command-line* (make-editable ""
                                      command-line-char
                                      command-line-key
                                      editable-clear!))

(define *command-line-mode* 'normal)

(define (command-line-pos)
  (cons (- (LINES) 1) 1))

(define (command-line-init!)
  (set-input-mode! 'edit-mode *command-line* (command-line-pos))
  (history-init!)
  (command-line-changed!))

(define (enter-eval-mode)
  (set! *command-line-mode* 'eval)
  (command-line-init!))

(define (enter-search-mode)
  (set! *command-line-mode* 'search)
  (command-line-init!))

(define (command-line-mode) *command-line-mode*)
