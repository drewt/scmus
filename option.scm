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

(require-extension ncurses) ; color constants

(declare (unit option)
         (uses format ui-curses)
         (export get-option set-option!))

;; An option is a value with associated get/set! functions.
;; The get/set! functions may be set to *option-value and
;; *option-value-set! if no extra processing is needed.
(define-record-type option
  (make-option accessor mutator value)
  option?
  (accessor option-get)
  (mutator option-set)
  (value *option-value *option-value-set!))

(define (get-option name)
  (let ((option (alist-ref name *options*)))
    (if option
      ((option-get option) option))))

(define (set-option! name value)
  (let ((option (alist-ref name *options*)))
    (if option
      ((option-set option) option value))))

(define (color-symbol? sym)
  (case sym
    ((default black red green yellow blue magenta cyan gray dark-gray
      light-red light-green light-yellow light-blue light-magenta light-cyan
      white) #t)
    (else    #f)))

(define (color-set! option value)
  (when (or (and (integer? value) (< value 256))
            (and (symbol? value) (color-symbol? value)))
    (*option-value-set! option value)
    (register-event! 'color-changed)))

(define (format-set! option value)
  (if (string? value)
    (let ((fmt (string->list value)))
      (when (format-string-valid? fmt)
        (*option-value-set! option (process-format fmt))
        (register-event! 'format-changed)))))

;; generates an alist entry for *options*
(define (option-spec name accessor mutator)
  (cons name (make-option accessor
                          mutator
                          (alist-ref name *default-options*))))

;; alist associating option names with default values
(define *default-options*
  (list
    (cons 'color-cmdline-attr              'default)
    (cons 'color-cmdline-bg                'default)
    (cons 'color-cmdline-fg                'default)
    (cons 'color-cur-sel-attr              'default)
    (cons 'color-error                     'red)
    (cons 'color-info                      'yellow)
    (cons 'color-statusline-attr           'default)
    (cons 'color-statusline-bg             'white)
    (cons 'color-statusline-fg             'black)
    (cons 'color-titleline-attr            'default)
    (cons 'color-titleline-bg              'blue)
    (cons 'color-titleline-fg              'white)
    (cons 'color-win-attr                  'default)
    (cons 'color-win-bg                    'default)
    (cons 'color-win-cur                   'yellow)
    (cons 'color-win-cur-sel-attr          'default)
    (cons 'color-win-cur-sel-bg            'blue)
    (cons 'color-win-cur-sel-fg            'yellow)
    (cons 'color-win-dir                   'blue)
    (cons 'color-win-fg                    'default)
    (cons 'color-win-sel-attr              'default)
    (cons 'color-win-sel-bg                'blue)
    (cons 'color-win-sel-fg                'white)
    (cons 'color-win-title-attr            'default)
    (cons 'color-win-title-bg              'blue)
    (cons 'color-win-title-fg              'white)
    (cons 'format-current
          (process-format (string->list "~a - ~l ~n. ~t~= ~y")))
    (cons 'format-status
          (process-format (string->list "~P ~p / ~d - ~T vol: ~v~= ~S~R~r~C")))
    (cons 'format-library
          (process-format (string->list "~-25%a ~3n. ~t~= ~-4y ~d")))
    (cons 'format-queue
          (process-format (string->list "~-25%a ~3n. ~t~= ~-4y ~d")))
    (cons 'format-queue-title
          (process-format (string->list "Play Queue - ~{queue-length} tracks")))))

;; alist associating option names with options
(define *options*
  (list
    (option-spec 'color-cmdline-attr *option-value color-set!)
    (option-spec 'color-cmdline-bg *option-value color-set!)
    (option-spec 'color-cmdline-fg *option-value color-set!)
    (option-spec 'color-cur-sel-attr *option-value color-set!)
    (option-spec 'color-error *option-value color-set!)
    (option-spec 'color-info *option-value color-set!)
    (option-spec 'color-statusline-attr *option-value color-set!)
    (option-spec 'color-statusline-bg *option-value color-set!)
    (option-spec 'color-statusline-fg *option-value color-set!)
    (option-spec 'color-titleline-attr *option-value color-set!)
    (option-spec 'color-titleline-bg *option-value color-set!)
    (option-spec 'color-titleline-fg *option-value color-set!)
    (option-spec 'color-win-attr *option-value color-set!)
    (option-spec 'color-win-bg *option-value color-set!)
    (option-spec 'color-win-cur *option-value color-set!)
    (option-spec 'color-win-cur-sel-attr *option-value color-set!)
    (option-spec 'color-win-cur-sel-bg *option-value color-set!)
    (option-spec 'color-win-cur-sel-fg *option-value color-set!)
    (option-spec 'color-win-dir *option-value color-set!)
    (option-spec 'color-win-fg *option-value color-set!)
    (option-spec 'color-win-sel-attr *option-value color-set!)
    (option-spec 'color-win-sel-bg *option-value color-set!)
    (option-spec 'color-win-sel-fg *option-value color-set!)
    (option-spec 'color-win-title-attr *option-value color-set!)
    (option-spec 'color-win-title-bg *option-value color-set!)
    (option-spec 'color-win-title-fg *option-value color-set!)
    (option-spec 'format-current *option-value format-set!)
    (option-spec 'format-status *option-value format-set!)
    (option-spec 'format-library *option-value format-set!)
    (option-spec 'format-queue *option-value format-set!)
    (option-spec 'format-queue-title *option-value format-set!)))
