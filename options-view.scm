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

(declare (export make-options-view option-edit!))

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

(: option-edit! (window -> undefined))
(define (option-edit! window)
  (text-input-begin (split-pane-right-child (window-selected window)) steal-focus: #t))

(define (option-commit-edit! widget)
  (let ((text (text-input-get-text widget)))
    (handle-exceptions e (begin (scmus-error-set! e) #f)
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

(define-view options
  (make-frame 'body   (make-window 'data       (make-options-data)
                                   'activate   option-edit!
                                   'edit       option-edit!
                                   'format     *key-value-format*
                                   'cursed     CURSED-WIN
                                   'cursed-fn (win-cursed-fn))
              'header (make-text " Options" 'cursed CURSED-WIN-TITLE)))

(define-event-handler (option-data-changed) ()
  (let ((window (get-window 'options)))
    (set! (window-data window)
      (make-options-data))))
