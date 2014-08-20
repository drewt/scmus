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

(declare (unit option)
         (uses format ncurses ui-curses)
         (export get-option set-option! *options* option-get option-set!
                 option-string write-config!))

;; An option is a value with associated get/set! functions.
;; The get/set! functions may be set to option-value and
;; option-value-set! if no extra processing is needed.
(define-record-type option
  (make-option accessor mutator stringifier value)
  option?
  (accessor option-accessor)
  (mutator option-mutator)
  (stringifier option-stringifier)
  (value option-value option-value-set!))

(define (option-get option)
  ((option-accessor option) option))

(define (option-set! option value)
  ((option-mutator option) option value)
  (register-event! 'option-data-changed))

(define (option-string option)
  ((option-stringifier option) option))

(define (get-option name)
  (let ((option (alist-ref name *options*)))
    (if option
      (option-get option))))

(define (set-option! name value)
  (let ((option (alist-ref name *options*)))
    (if option
      (option-set! option value))))

(define (mpd-address-set! option value)
  (when (string? value)
    (option-value-set! option value)))

(define (mpd-port-set! option value)
  (when (port-valid? value)
    (option-value-set! option value)))

(define (mpd-password-set! option value)
  (when (or (not value) (string? value))
    (option-value-set! option value)))

(define (update-interval-set! option value)
  (when (and (number? value)
             (positive? value))
    (option-value-set! option value)))

(define (color-symbol? sym)
  (case sym
    ((default black red green yellow blue magenta cyan gray dark-gray
      light-red light-green light-yellow light-blue light-magenta light-cyan
      white) #t)
    (else    #f)))

(define (attr-valid? attr)
  (case attr
    ((default normal underline reverse blink bold dim altcharset invis
      attributes chartext color standout protect left right low top vertical)
          #t)
    (else #f)))

(define (color-valid? value)
  (or (and (integer? value) (< value 256))
      (and (symbol? value) (color-symbol? value))))

(define (color-set! option value)
  (when (and (list? value)
             (= (length value) 3)
             (attr-valid? (car value))
             (color-valid? (cadr value))
             (color-valid? (caddr value)))
    (option-value-set! option value)
    (register-event! 'color-changed)))

(define (format-set! option value)
  (if (string? value)
    (let ((fmt (string->list value)))
      (when (format-string-valid? fmt)
        (option-value-set! option (cons (string-append "\"" value "\"")
                                        (process-format fmt)))
        (register-event! 'format-changed)))))

(define (format-get option)
  (cdr (option-value option)))

(define (format-stringify option)
  (car (option-value option)))

(define (stringify option)
  (let ((value (option-get option)))
    (format "~s" value)))

(define (format-value fmt)
  (cons (string-append "\"" fmt "\"") (process-format (string->list fmt))))

;; generates an alist entry for *options*
(define (option-spec name accessor mutator #!optional (stringifier stringify))
  (cons name (make-option accessor
                          mutator
                          stringifier
                          (alist-ref name *default-options*))))

;; alist associating option names with default values
(define *default-options*
  (list
    (cons 'mpd-address                     "localhost")
    (cons 'mpd-port                        6600)
    (cons 'mpd-password                    #f)
    (cons 'status-update-interval          0.5)
    (cons 'color-cmdline                  '(default default default))
    (cons 'color-error                    '(default default red))
    (cons 'color-info                     '(default default yellow))
    (cons 'color-statusline               '(default white black))
    (cons 'color-titleline                '(default blue white))
    (cons 'color-win                      '(default default default))
    (cons 'color-win-cur                  '(default default yellow))
    (cons 'color-win-cur-sel              '(default blue yellow))
    (cons 'color-win-sel                  '(default blue white))
    (cons 'color-win-marked               '(default blue white))
    (cons 'color-win-title                '(default blue white))
    (cons 'format-current
          (format-value "~a - ~l ~n. ~t~= ~y"))
    (cons 'format-status
          (format-value "~P ~p / ~d - ~T vol: ~v~= ~S~R~r~C"))
    (cons 'format-library
          (format-value "~-25%a ~3n. ~t~= ~-4y ~d"))
    (cons 'format-queue
          (format-value "~-25%a ~3n. ~t~= ~-4y ~d"))
    (cons 'format-queue-title
          (format-value "Play Queue - ~{queue-length} tracks"))))

;; alist associating option names with options
(define *options*
  (list
    (option-spec 'mpd-address option-value mpd-address-set!)
    (option-spec 'mpd-port option-value mpd-port-set!)
    (option-spec 'mpd-password option-value mpd-password-set!)
    (option-spec 'status-update-interval option-value update-interval-set!)
    (option-spec 'color-cmdline option-value color-set!)
    (option-spec 'color-error option-value color-set!)
    (option-spec 'color-info option-value color-set!)
    (option-spec 'color-statusline option-value color-set!)
    (option-spec 'color-titleline option-value color-set!)
    (option-spec 'color-win option-value color-set!)
    (option-spec 'color-win-cur option-value color-set!)
    (option-spec 'color-win-cur-sel option-value color-set!)
    (option-spec 'color-win-sel option-value color-set!)
    (option-spec 'color-win-marked option-value color-set!)
    (option-spec 'color-win-title option-value color-set!)
    (option-spec 'format-current format-get format-set! format-stringify)
    (option-spec 'format-status format-get format-set! format-stringify)
    (option-spec 'format-library format-get format-set! format-stringify)
    (option-spec 'format-queue format-get format-set! format-stringify)
    (option-spec 'format-queue-title format-get format-set! format-stringify)))

(define (write-config! path)
  (call-with-output-file path
    (lambda (out)
      (let loop ((options *options*))
        (unless (null? options)
          (display `(set-option! ',(caar options) ,(option-string (cdar options))) out)
          (newline out)
          (loop (cdr options)))))))
