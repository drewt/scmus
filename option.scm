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
         (uses format)
         (export get-option
                 set-option!))

;; An option is a value with associated get/set! functions.
;; The get/set! functions may be set to *option-value and
;; *option-set-value! if no extra processing is needed.
(define-record-type option
  (make-option accessor mutator value)
  option?
  (accessor option-get)
  (mutator option-set)
  (value *option-value *option-set-value!))

(define (get-option name)
  (let ((option (alist-ref name *options*)))
    ((option-get option) option)))

(define (set-option! name value)
  (let ((option (alist-ref name *options*)))
    ((option-set option) option value)))

(define (format-set! option value)
  (if (and (string? value)
           (format-valid? value))
    (*option-set-value! option (process-format value))))

;; generates an alist entry for *options*
(define (option-spec name accessor mutator)
  (cons name (make-option accessor
                          mutator
                          (alist-ref name *default-options*))))

;; alist associating option names with default values
(define *default-options*
  (list
    (cons 'format-current
          (process-format (string->list "~a - ~l ~n. ~t~= ~y")))
    (cons 'format-status
          (process-format (string->list "~P ~p / ~d - ~T vol: ~v")))
    (cons 'format-queue
          (process-format (string->list "~n. ~t~= ~y ~d")))
    ))

;; alist associating option names with options
(define *options*
  (list
    (option-spec 'format-current *option-value format-set!)
    (option-spec 'format-status *option-value format-set!)
    (option-spec 'format-queue *option-value format-set!)))
