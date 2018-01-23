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

(declare (unit command-line)
         (uses editable event input iter ncurses))

(module scmus.command-line (command-line-clear!
                            command-line-print-info!
                            command-line-cursor-pos-set!
                            command-line-get-string
                            command-line-mode
                            command-line-print-error!
                            command-line-text
                            command-line-text-set!)

  (import scmus.base scmus.editable scmus.event scmus.input iter ncurses)

  ;; history {{{

  (define history
    (let ((histories '()))
      (getter-with-setter
        (lambda ()
          (let ((this-history (alist-ref *command-line-mode* histories)))
            (if this-history
              this-history
              (let ((this-history (iter)))
                (set! histories (alist-update! *command-line-mode* this-history histories))
                this-history))))
        (lambda (x)
          (set! histories (alist-update! *command-line-mode* x histories))))))

  (: history-next! (-> undefined))
  (define (history-next!)
    (set! (history) (iter-next (history))))

  (: history-prev! (-> undefined))
  (define (history-prev!)
    (set! (history) (iter-prev (history))))

  (: history-add! (* -> undefined))
  (define (history-add! elm)
    (unless (string=? elm "")
      (iter-add-head! (history) elm)))

  (: history-data (-> *))
  (define (history-data)
    (let ((iter (history)))
      (if (iter-head? iter)
        ""
        (iter-data iter))))

  (: history-reset! (-> undefined))
  (define (history-reset!)
    (set! (history) (iter-head (history))))

  ;; history }}}

  (: command-line-print-info! (string -> undefined))
  (define (command-line-print-info! str)
    (case *command-line-mode*
      ((eval search) (void))
      (else (editable-text-set! *command-line* str)
            (set! *command-line-mode* 'info)
            (command-line-changed!)
            str)))

  (: command-line-print-error! (string -> undefined))
  (define (command-line-print-error! str)
    (case *command-line-mode*
      ((eval search) (void))
      (else (editable-text-set! *command-line* str)
            (set! *command-line-mode* 'error)
            (command-line-changed!)
            str)))

  (: command-line-changed! thunk)
  (define (command-line-changed!)
    (register-event! 'command-line-changed))

  (: command-line-empty? (-> boolean))
  (define (command-line-empty?)
    (= 0 (command-line-length)))

  (: command-line-clear! thunk)
  (define (command-line-clear!)
    (editable-clear! *command-line*)
    (command-line-changed!))

  (: command-line-text (-> string))
  (define (command-line-text)
    (editable-text *command-line*))

  (: command-line-text-set! (string -> undefined))
  (define (command-line-text-set! str)
    (editable-text-set! *command-line* str))

  (: command-line-cursor-pos (-> fixnum))
  (define (command-line-cursor-pos)
    (+ 1 (editable-cursor-pos *command-line*)))

  (: command-line-cursor-pos-set! (fixnum -> undefined))
  (define (command-line-cursor-pos-set! index)
    (editable-cursor-pos-set! *command-line* index))

  (: command-line-length (-> fixnum))
  (define (command-line-length)
    (editable-length *command-line*))

  (: command-line-leave (editable -> undefined))
  (define (command-line-leave editable)
    (history-add! (editable-text editable))
    (history-reset!)
    (editable-clear! editable)
    (set! *command-line-mode* 'normal)
    (set! *command-line-callback* void)
    (set-input-mode! 'normal-mode))

  (: command-line-commit! (editable -> undefined))
  (define (command-line-commit! editable)
    (let ((cmdline (editable-text editable))
          (callback *command-line-callback*)
          (mode *command-line-mode*))
      (command-line-leave editable)
      (callback cmdline)))

  (: command-line-char (editable char -> undefined))
  (define (command-line-char editable ch)
    (case ch
      ((#\newline)
        (command-line-commit! editable))
      ((#\esc)
        (let ((callback *command-line-callback*))
          (command-line-leave editable)
          (callback #f)))
      ((#\backspace #\delete)
        (if (= 0 (editable-length editable))
          (command-line-leave editable)
          (editable-default-char-handler editable ch)))
      (else
        (editable-default-char-handler editable ch)))
    (command-line-changed!))

  (: command-line-key (editable fixnum -> undefined))
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

  (: *command-line* editable)
  (define *command-line* (make-editable ""
                                        command-line-char
                                        command-line-key
                                        editable-clear!))

  (: *command-line-mode* symbol)
  (define *command-line-mode* 'normal)

  (: *command-line-callback* ((or string boolean) -> *))
  (define *command-line-callback* void)

  (: command-line-pos (-> (pair fixnum fixnum)))
  (define (command-line-pos)
    (cons (- (LINES) 1) 1))

  (: command-line-init! thunk)
  (define (command-line-init!)
    (set-input-mode! 'edit-mode *command-line* (command-line-pos))
    (command-line-changed!))


  (: command-line-mode (-> symbol))
  (define (command-line-mode) *command-line-mode*)

  (: command-line-get-string (symbol ((or string boolean) -> *) -> undefined))
  (define (command-line-get-string mode then)
    (set! *command-line-mode* mode)
    (set! *command-line-callback* then)
    (command-line-init!)))
