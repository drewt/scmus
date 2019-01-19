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

(declare (hide))

(import drewt.ncurses
        scmus.base
        scmus.error
        scmus.event
        scmus.option
        scmus.tui
        scmus.view
        scmus.widgets)

;; TODO: mark only changed option as damaged
(: option-changed! thunk)
(define (option-changed!)
  (widget-damaged! (get-view 'options)))

(define (option-row-name row)
  (string->symbol (string-trim (car (text-text (split-pane-left-child row))))))

(define (option-commit-edit! text)
  (let ((widget (current-event-source)))
    (handle-exceptions e (begin (scmus-error e) #f)
      (set-option! (option-row-name (widget-parent widget))
                   (with-input-from-string text read)))
    (text-input-set-text! widget
      (option->string (*get-option (option-row-name (widget-parent widget)))))))

(: make-options-data (-> list))
(define (make-options-data)
  (map (lambda (option)
         ; TODO: instead of using a <split-pane>, use a text-input with a prefix
         ;       and set the prefix length to half the screen size
         (let ((input (make-text-input (option->string (cdr option)) "")))
           (add-listener input 'commit option-commit-edit!)
           (make-split-pane (make-text (string-append " " (symbol->string (car option))))
                            input)))
       (options)))

(define-class <options-window> (<window>))

(define-method (widget-edit (window <options-window>))
  (unless (list-box-empty? window)
    (text-input-begin (split-pane-right-child (list-box-selected window)) steal-focus: #t)))

(define-method (widget-activate (window <options-window>))
  (widget-edit window))

(define *options-window*
  (make <options-window>
        'data       (make-options-data)
        'cursed     CURSED-WIN
        'cursed-fun (win-cursed-fun)))

(define-view options
  (make-frame 'body   *options-window*
              'header (make-text " Options" 'cursed CURSED-WIN-TITLE)))

(add-listener/global 'option-data-changed
  (lambda ()
    (set! (list-box-data *options-window*) (make-options-data))))
