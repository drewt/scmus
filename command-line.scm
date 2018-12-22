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

(module scmus.command-line (command-line-widget
                            command-line-print-info!
                            command-line-print-error!
                            command-line-clear!
                            command-line-text
                            command-line-text-set!
                            command-line-cursor-pos-set!
                            command-line-get-string
                            make-command-line-mode)
  (import coops
          drewt.iter
          drewt.ncurses
          scmus.base
          scmus.event
          scmus.tui)

  (define current-command-line-mode (make-parameter #f))

  (define-record-type command-line-mode
    (%make-command-line-mode prefix callback history)
    command-line-mode?
    (prefix   command-line-mode-prefix)
    (callback command-line-mode-callback)
    (history  command-line-mode-history
              command-line-mode-history-set!))

  (define (make-command-line-mode prefix callback)
    (%make-command-line-mode prefix callback (iter)))

  ;; history {{{

  (define history
    (getter-with-setter
      (lambda ()
        (assert (current-command-line-mode) "history")
        (command-line-mode-history (current-command-line-mode)))
      (lambda (x)
        (assert (current-command-line-mode) "(setter history")
        (command-line-mode-history-set! (current-command-line-mode) x))))

  (define (history-next!)
    (set! (history) (iter-next (history))))

  (define (history-prev!)
    (set! (history) (iter-prev (history))))

  (define (history-add! elm)
    (unless (string=? elm "")
      (iter-add-head! (history) elm)))

  (define (history-data)
    (let ((iter (history)))
      (if (iter-head? iter)
        ""
        (iter-data iter))))

  (define (history-reset!)
    (set! (history) (iter-head (history))))

  ;; history }}}

  (define-class <command-line> (<text-input>))

  (define-method (handle-input (widget <command-line>) input event)
    (key-case input
      ((KEY_UP)
        (history-next!)
        (text-input-set-text! widget (history-data)))
      ((KEY_DOWN)
        (history-prev!)
        (text-input-set-text! widget (history-data)))
      ((KEY_BACKSPACE)
        (if (and (text-input-editing? widget)
                 (null? (text-input-text widget)))
          (text-input-cancel widget)
          (call-next-method)))
      (else (call-next-method))))

  (define command-line-widget
    (make <command-line>
      'cursed   CURSED-CMDLINE
      'prefix   " "
      'on-begin (lambda (w)
                  (set! (widget-cursed w) CURSED-CMDLINE))
      'on-leave (lambda (w)
                  (current-command-line-mode #f)
                  (set! (text-input-prefix w) " ")
                  (set! (text-input-on-commit w) (lambda (w) #f))
                  (set! (text-input-on-cancel w) (lambda (w) #f)))))

  (define (command-line-print-info! str)
    (unless (text-input-editing? command-line-widget)
      (set! (widget-cursed command-line-widget) CURSED-INFO)
      (text-input-set-text! command-line-widget str)))

  (define (command-line-print-error! str)
    (unless (text-input-editing? command-line-widget)
      (set! (widget-cursed command-line-widget) CURSED-ERROR)
      (text-input-set-text! command-line-widget str)))

  (define (command-line-clear!)
    (set! (text-input-text command-line-widget) '()))

  (define (command-line-text)
    (text-input-get-text command-line-widget))

  (define (command-line-text-set! str)
    (text-input-set-text! command-line-widget str))

  (define (command-line-cursor-pos-set! n)
    (text-input-set-cursor-pos! command-line-widget n))

  (define (command-line-get-string mode #!optional (text "") (cursor-pos 0))
    (current-command-line-mode mode)
    (set! (text-input-prefix command-line-widget)
      (command-line-mode-prefix mode))
    (set! (text-input-on-commit command-line-widget)
      (lambda (w)
        (let ((text (text-input-get-text w)))
          (text-input-set-text! w "")
          (history-add! text)
          ((command-line-mode-callback mode) text))))
    (set! (text-input-on-cancel command-line-widget)
      (lambda (w)
        (set! (text-input-prefix w) " ")
        (text-input-set-text! w "")))
    (text-input-set-text! command-line-widget text)
    (text-input-set-cursor-pos! command-line-widget cursor-pos)
    (text-input-begin command-line-widget steal-focus: #t)))
