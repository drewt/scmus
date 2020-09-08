;;
;; Copyright 2014-2020 Drew Thoreson
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

(require-extension srfi-69)

(module (scmus option)
    (<option>
     option-value
     option->string
     make-option
     *get-option
     get-option
     set-option!
     options
     register-option!
     add-option-listener
     write-config!)
  (import (srfi 69)
          coops
          (scmus base)
          (scmus event))

  (define-class <option> (<event-source>)
    ((value     reader:   option-value)
     (validator initform: (lambda (_) #t)
                reader:   option-validator)
     (converter initform: values
                reader:   option-converter)
     (after     reader:   option-after
                initform: void)))

  (define (make-option value . rest)
    (let ((opt (apply make <option> rest)))
      (set! (option-value opt) value)
      opt))

  (define-method (option->string (option <option>))
    (format "~s" (option-value option)))

  (define-method ((setter option-value) (option <option>) value)
    (when ((option-validator option) value)
      (set! (slot-value option 'value) ((option-converter option) value))
      ((option-after option) value))
    ; FIXME: these should also go under the WHEN...
    ;        but options-view doesn't know that we failed
    (signal-event option 'option-changed args: (list option))
    (signal-event/global 'option-data-changed))

  (define (*get-option name)
    (hash-table-ref/default *options* name #f))

  (define *options* (make-hash-table))

  (define (options)
    (sort! (hash-table->alist *options*)
           (lambda (a b)
             (string<? (symbol->string (car a))
                       (symbol->string (car b))))))

  (define (register-option! name option)
    (hash-table-set! *options* name option))

  (define (get-option name)
    (let ((option (*get-option name)))
      (if option
        (option-value option)
        (error "Option not found" name))))

  (define (set-option! name value)
    (let ((option (*get-option name)))
      (if option
        (set! (option-value option) value)
        (error "Option not found" name))))

  (define (add-option-listener name listener)
    (add-listener (*get-option name) 'option-changed listener))

  (define (write-config! path)
    (call-with-output-file path
      (lambda (out)
        (let loop ((opts (options)))
          (unless (null? opts)
            (display `(set-option! ',(caar opts)
                                   ',(option->string (cdar opts)))
                     out)
            (newline out)
            (loop (cdr opts))))))))
