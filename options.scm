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

(declare (unit options)
         (uses event format option)
         (export get-format))

(import scmus.base scmus.event scmus.format scmus.option)

;; {{{ Miscellaneous
(: boolean-set! option-setter)
(define (boolean-set! option value)
  (option-value-set! option (if value #t #f)))

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

(register-option! 'mpd-address "localhost" mpd-address-set!)
(register-option! 'mpd-port 6600 mpd-port-set!)
(register-option! 'mpd-password #f mpd-password-set!)
(register-option! 'status-update-interval 0.5 update-interval-set!)
(register-option! 'eval-mode-print #f boolean-set!)
;; }}} Miscellaneous
;; {{{ Colors
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

(define (register-colors! name colors)
  (register-option! name colors color-set!))

(register-colors! 'color-cmdline     '(default default default))
(register-colors! 'color-error       '(default default red))
(register-colors! 'color-info        '(default default yellow))
(register-colors! 'color-statusline  '(default white   black))
(register-colors! 'color-titleline   '(default blue    white))
(register-colors! 'color-win         '(default default default))
(register-colors! 'color-win-cur     '(default default yellow))
(register-colors! 'color-win-cur-sel '(default blue    yellow))
(register-colors! 'color-win-sel     '(default blue    white))
(register-colors! 'color-win-marked  '(default blue    white))
(register-colors! 'color-win-title   '(default blue    white))
;; }}} Colors
;; {{{ Format Strings
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

(define *default-track-format* "~-25%a ~3n. ~t~= ~-4y ~d")

(define (register-format! name fmt)
  (register-option! name (format-values fmt)
    format-set!
    format-get
    format-stringify))

(register-format! 'format-separator        "~{text}")
(register-format! 'format-current          " ~a - ~l ~n. ~t~= ~y")
(register-format! 'format-status           " ~P ~p / ~d - ~T vol: ~v~= ~S~R~r~C ")
(register-format! 'format-library-playlist "~{playlist}")
(register-format! 'format-library-artist   "~{artist}")
(register-format! 'format-library-album    "~{album}")
(register-format! 'format-library-file     *default-track-format*)
(register-format! 'format-library-metadata "~-50%{tag} ~{value}")
(register-format! 'format-queue            *default-track-format*)
(register-format! 'format-browser-file     "~{path}")
(register-format! 'format-browser-dir      "~{directory}/")
(register-format! 'format-browser-playlist "[~{playlist}]")
(register-format! 'format-browser-metadata "~-50%{tag} ~{value}")
(register-format! 'format-search-file      *default-track-format*)
;; }}} Format Strings
