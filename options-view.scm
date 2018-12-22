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

(import drewt.ncurses)
(import scmus.base
        scmus.error
        scmus.event
        scmus.option
        scmus.tui
        scmus.widgets)

;; TODO: mark only changed option as damaged
(: option-changed! thunk)
(define (option-changed!)
  (widget-damaged! (get-view 'options)))

(define (option-commit-edit! widget)
  (let ((text (text-input-get-text widget)))
    (handle-exceptions e (begin (scmus-error e) #f)
      (set-option! (scheme-text-expr (split-pane-left-child (widget-parent widget)))
                   (with-input-from-string (text-input-get-text widget) read)))))

(: make-options-data (-> list))
(define (make-options-data)
  (map (lambda (option)
         ; TODO: instead of using a <split-pane>, use a text-input with a prefix
         ;       and set the prefix length to half the screen size
         (make-split-pane (make-scheme-text (car option))
                          (make-text-input (option-string (cdr option)) ""
                                           'on-commit option-commit-edit!)))
       (options)))

(define-class <options-window> (<window>))

(define-method (widget-edit (window <options-window>))
  (unless (window-empty? window)
    (text-input-begin (split-pane-right-child (window-selected window)) steal-focus: #t)))

(define-method (widget-activate (window <options-window>))
  (widget-edit window))

(define *options-window*
  (make <options-window>
        'data       (make-options-data)
        'cursed     CURSED-WIN
        'cursed-fn (win-cursed-fn)))

(define-view options
  (make-frame 'body   *options-window*
              'header (make-text " Options" 'cursed CURSED-WIN-TITLE)))

(define-event-handler (option-data-changed) ()
  (set! (window-data *options-window*) (make-options-data)))
