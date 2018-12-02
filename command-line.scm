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
                            command-line-get-string)
  (import coops)
  (import drewt.iter drewt.ncurses)
  (import scmus.base scmus.editable scmus.event scmus.input scmus.tui)

  (define *command-line-mode* 'normal)

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

  (define-class <command-line> (<text-input>))

  (define-method (handle-input (widget <command-line>) input)
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
                  (set! *command-line-mode* 'normal)
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

  (define (command-line-get-string mode then #!optional (text "") (cursor-pos 0))
    (set! (text-input-prefix command-line-widget)
      (case mode
        ((command) ":")
        ((eval)    "$")
        ((search)  "/")))
    (set! *command-line-mode* mode)
    ; XXX: we focus command-line-widget, then restore the previous focus
    (let ((old-focus (widget-focus (widget-root command-line-widget))))
      (set! (text-input-on-commit command-line-widget)
        (lambda (w)
          (let ((text (text-input-get-text w)))
            (set-focus! old-focus)
            (text-input-set-text! w "")
            (history-add! text)
            (then text))))
      (set! (text-input-on-cancel command-line-widget)
        (lambda (w)
          (set-focus! old-focus)
          (set! (text-input-prefix w) " ")
          (text-input-set-text! w "")))
      (set-focus! command-line-widget)
      (text-input-set-text! command-line-widget text)
      (text-input-set-cursor-pos! command-line-widget cursor-pos)
      (text-input-begin command-line-widget))))
