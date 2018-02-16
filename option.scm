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

(require-extension srfi-69)

(module scmus.option (get-option
                      set-option!
                      options
                      option-get
                      option-set!
                      option-string
                      option-value
                      option-value-set!
                      write-config!
                      register-option!)
  (import srfi-69)
  (import scmus.base scmus.event)

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
    (let ((option (hash-table-ref/default *options* name #f)))
      (if option
        (option-get option))))

  (: set-option! (symbol * -> undefined))
  (define (set-option! name value)
    (let ((option (hash-table-ref/default *options* name #f)))
      (if option
        (option-set! option value))))

  (: stringify (option -> string))
  (define (stringify option)
    (let ((value (option-get option)))
      (format "~s" value)))

  (define *options* (make-hash-table))

  (define (register-option! name value #!optional (setter option-value-set!)
                                                  (getter option-value)
                                                  (to-string stringify))
    (hash-table-set! *options* name
      (make-option getter setter to-string value)))

  (: options (-> (list-of (pair symbol option))))
  (define (options)
    (sort! (hash-table->alist *options*)
           (lambda (a b)
             (string<? (symbol->string (car a))
                       (symbol->string (car b))))))

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
            (loop (cdr options))))))))
