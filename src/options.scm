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

(import scmus.base
        scmus.event
        scmus.format
        scmus.option
        scmus.tui)

;; {{{ Miscellaneous
(register-option! 'mpd-address
  (make-option "localhost" 'validator string?))

(register-option! 'mpd-port
  (make-option 6600 'validator (lambda (port)
                                 (or (not port)
                                     (and (integer? port)
                                          (< port 65536))))))
(register-option! 'mpd-password
  (make-option #f 'validator (lambda (pass)
                               (or (not pass)
                                   (string? pass)))))

(register-option! 'status-update-interval
  (make-option 1.5 'validator (lambda (n)
                                (and (number? n)
                                     (positive? n)))))

(register-option! 'eval-mode-print
  (make-option #f 'converter (lambda (v) (not (not v)))))

(register-option! 'enable-mouse
  (make-option #t 'converter (lambda (v) (not (not v)))
                  'after     (lambda (v) (enable-mouse v))))
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

(define (register-colors! name colors)
  (register-option! name
    (make-option colors 'validator (lambda (colors)
                                     (and (list? colors)
                                          (= (length colors) 3)
                                          (attr-valid? (first colors))
                                          (color-valid? (second colors))
                                          (color-valid? (third colors))))
                        'after     (lambda (_)
                                     (signal-event/global 'color-changed)))))

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
(define-class <format-option> (<option>)
  ((string accessor: format-option-string)))

(define-method ((setter option-value) after: (option <format-option>) value)
  (set! (format-option-string option) value))

(define-method (option->string (option <format-option>))
  (format "~s" (format-option-string option)))

(define (format-valid? fmt)
  (and (string? fmt) (format-string-valid? fmt)))

(define (register-format! name fmt)
  (let ((opt (make <format-option> 'validator format-valid?
                                   'converter compile-format-string
                                   'after     (lambda (_)
                                                (signal-event/global 'format-changed)))))
    (set! (option-value opt) fmt)
    (register-option! name opt)))

(define *default-track-format* " ~-25%a ~3n. ~t~= ~-4y ~d ")

(register-format! 'format-separator        " ~{text}")
(register-format! 'format-current          " ~a - ~l ~n. ~t~= ~y ")
(register-format! 'format-status           " ~P ~p / ~d - ~T vol: ~v~= ~S~R~r~C ")
(register-format! 'format-library-playlist " ~{playlist}")
(register-format! 'format-library-artist   " ~{artist}")
(register-format! 'format-library-album    " ~{album}")
(register-format! 'format-library-file     *default-track-format*)
(register-format! 'format-library-metadata " ~-50%{tag} ~{value}")
(register-format! 'format-queue            *default-track-format*)
(register-format! 'format-playlist         *default-track-format*)
(register-format! 'format-browser-file     " ~{path}")
(register-format! 'format-browser-dir      " ~{directory}/")
(register-format! 'format-browser-playlist " [~{playlist}]")
(register-format! 'format-browser-metadata " ~-50%{tag} ~{value}")
(register-format! 'format-search-file      *default-track-format*)
;; }}} Format Strings
