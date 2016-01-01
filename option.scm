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
         (uses event format ncurses)
         (export get-option set-option! options option-get option-set!
                 option-string get-format write-config!))

(define-type option-getter (option -> *))
(define-type option-setter (option * -> undefined))

;; An option is a value with associated get/set! functions.
;; The get/set! functions may be set to option-value and
;; option-value-set! if no extra processing is needed.
(: make-option (option-getter option-setter (option -> string) * -> option))
(: option? predicate)
(: option-accessor (option -> option-getter))
(: option-mutator (option -> option-setter))
(: option-stringifier (option -> (option -> string)))
(: option-value option-getter)
(: option-value-set! option-setter)
(define-record-type option
  (make-option accessor mutator stringifier value)
  option?
  (accessor option-accessor)
  (mutator option-mutator)
  (stringifier option-stringifier)
  (value option-value option-value-set!))

(: option-get option-getter)
(define (option-get option)
  ((option-accessor option) option))

(: option-set! option-setter)
(define (option-set! option value)
  ((option-mutator option) option value)
  (register-event! 'option-data-changed))

(: option-string (option -> string))
(define (option-string option)
  ((option-stringifier option) option))

(: get-option (symbol -> *))
(define (get-option name)
  (let ((option (alist-ref name *options*)))
    (if option
      (option-get option))))

(: set-option! (symbol * -> undefined))
(define (set-option! name value)
  (let ((option (alist-ref name *options*)))
    (if option
      (option-set! option value))))

(: mpd-address-set! option-setter)
(define (mpd-address-set! option value)
  (when (string? value)
    (option-value-set! option value)))

(: mpd-port-set! option-setter)
(define (mpd-port-set! option value)
  (when (or (not value) (port-valid? value))
    (option-value-set! option value)))

(: mpd-password-set! option-setter)
(define (mpd-password-set! option value)
  (when (or (not value) (string? value))
    (option-value-set! option value)))

(: update-interval-set! option-setter)
(define (update-interval-set! option value)
  (when (and (number? value)
             (positive? value))
    (option-value-set! option value)))

(: color-symbol? predicate)
(define (color-symbol? sym)
  (case sym
    ((default black red green yellow blue magenta cyan gray dark-gray
      light-red light-green light-yellow light-blue light-magenta light-cyan
      white) #t)
    (else    #f)))

(: attr-valid? predicate)
(define (attr-valid? attr)
  (case attr
    ((default normal underline reverse blink bold dim altcharset invis
      attributes chartext color standout protect left right low top vertical)
          #t)
    (else #f)))

(: color-valid? predicate)
(define (color-valid? value)
  (or (and (integer? value) (< value 256))
      (and (symbol? value) (color-symbol? value))))

(: color-set! option-setter)
(define (color-set! option value)
  (when (and (list? value)
             (= (length value) 3)
             (attr-valid? (car value))
             (color-valid? (cadr value))
             (color-valid? (caddr value)))
    (option-value-set! option value)
    (register-event! 'color-changed)))

(: format-set! option-setter)
(define (format-set! option value)
  (when (and (string? value) (format-string-valid? value))
    (option-value-set! option (format-values value))
    (register-event! 'format-changed)))

(: format-get (option -> string format-spec))
(define (format-get option)
  (let ((pair (option-value option)))
    (values (car pair) (cdr pair))))

(: format-stringify (option -> string))
(define (format-stringify option)
  (format "~s" (car (option-value option))))

(: format-values (string -> (pair string format-spec)))
(define (format-values fmt)
  (cons fmt (process-format fmt)))

(: get-format (symbol -> format-spec))
(define (get-format name)
  (nth-value 1 (get-option name)))

(: stringify (option -> string))
(define (stringify option)
  (let ((value (option-get option)))
    (format "~s" value)))

(define-type option-spec (pair symbol option))
;; Generates an alist entry for *options*.
;; XXX: CHICKEN doesn't like that format-get returns multiple values, so the
;;      accessor type is specified as procedure.
(: option-spec (symbol procedure option-setter #!optional (option -> string)
                  -> option-spec))
(define (option-spec name accessor mutator #!optional (stringifier stringify))
  (cons name (make-option accessor
                          mutator
                          stringifier
                          (alist-ref name *default-options*))))

(: color-option (symbol -> option-spec))
(define (color-option name)
  (option-spec name option-value color-set!))

(: format-option (symbol -> option-spec))
(define (format-option name)
  (option-spec name format-get format-set! format-stringify))

(: boolean-option (symbol -> option-spec))
(define (boolean-option name)
  (option-spec name option-value
               (lambda (option value)
                 (option-value-set! option (if value #t #f)))))

;; alist associating option names with default values
(: *default-options* (list-of (pair symbol *)))
(define *default-options*
  (list
    (cons 'mpd-address                    "localhost")
    (cons 'mpd-port                       6600)
    (cons 'mpd-password                   #f)
    (cons 'status-update-interval         0.5)
    (cons 'eval-mode-print                #f)
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
          (format-values " ~a - ~l ~n. ~t~= ~y "))
    (cons 'format-status
          (format-values " ~P ~p / ~d - ~T vol: ~v~= ~S~R~r~C "))
    (cons 'format-library
          (format-values "~-25%a ~3n. ~t~= ~-4y ~d"))
    (cons 'format-queue
          (format-values "~-25%a ~3n. ~t~= ~-4y ~d"))
    (cons 'format-browser-file
          (format-values "~{path}"))
    (cons 'format-browser-dir
          (format-values "~{directory}/"))
    (cons 'format-browser-playlist
          (format-values "[~{playlist}]"))))

;; alist associating option names with options
(: *options* (list-of option-spec))
(define *options*
  (list
    (option-spec    'mpd-address option-value mpd-address-set!)
    (option-spec    'mpd-port option-value mpd-port-set!)
    (option-spec    'mpd-password option-value mpd-password-set!)
    (option-spec    'status-update-interval option-value update-interval-set!)
    (boolean-option 'eval-mode-print)
    (color-option   'color-cmdline)
    (color-option   'color-error)
    (color-option   'color-info)
    (color-option   'color-statusline)
    (color-option   'color-titleline)
    (color-option   'color-win)
    (color-option   'color-win-cur)
    (color-option   'color-win-cur-sel)
    (color-option   'color-win-sel)
    (color-option   'color-win-marked)
    (color-option   'color-win-title)
    (format-option  'format-current)
    (format-option  'format-status)
    (format-option  'format-library)
    (format-option  'format-queue)
    (format-option  'format-browser-file)
    (format-option  'format-browser-dir)
    (format-option  'format-browser-playlist)))

(: options (-> (list-of option-spec)))
(define (options) *options*)

(: write-config! (string -> undefined))
(define (write-config! path)
  (call-with-output-file path
    (lambda (out)
      (let loop ((options *options*))
        (unless (null? options)
          (display `(set-option! ',(caar options)
                                 ',(option-string (cdar options)))
                   out)
          (newline out)
          (loop (cdr options)))))))
