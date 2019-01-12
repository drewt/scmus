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

(module drewt.getopt (mkopt process-opts usage store-true store-one store-all store-number)
  (import scheme chicken data-structures)

  (define-record-type cmdline-opt
    (*mkopt key names args doc store valid)
    cmdline-opt?
    (key opt-key)
    (names opt-names)
    (args opt-args)
    (doc opt-doc)
    (store opt-store)
    (valid opt-valid))

  (define (mkopt key names args doc #!optional (store store-true) (valid true))
    (*mkopt key names args doc store valid))

  (define (invalid-value opt val bail)
    (printf "Invalid value for ~a: ~a~n" opt val)
    (bail))

  (define (not-enough-arguments opt bail)
    (printf "Not enough arguments for ~a~n" opt)
    (bail))

  (define (true x) #t)

  (define (store-true opt args bail)
    (values #t args))

  (define (store-one opt args bail)
    (if (null? args)
      (not-enough-arguments opt bail)
      (values (car args) (cdr args))))

  (define (store-all opt args bail)
    (if (null? args)
      (not-enough-arguments opt bail)
      (values args '())))

  (define (store-number opt args bail)
    (if (null? args)
      (not-enough-arguments opt bail)
      (let ((int (string->number (car args))))
        (if int
          (values int (cdr args))
          (invalid-value opt (car args) bail)))))

  (define (process-opts args opts #!optional *bail)
    (define (get-opt opt)
      (let loop ((opts opts))
       (cond
         ((null? opts) #f)
         ((member opt (opt-names (car opts))) (car opts))
         (else (loop (cdr opts))))))
    (let ((bail (if *bail *bail (lambda () (usage opts)))))
      (let loop ((args args) (tab '()))
        (if (null? args)
          tab
          (let ((opt (get-opt (car args))))
            (if opt
              (let-values (((val rest) ((opt-store opt) (car args) (cdr args) bail)))
                (if ((opt-valid opt) val)
                  (loop rest (alist-update (opt-key opt) val tab))
                  (invalid-value (car args) val bail)))
              (begin
                (printf "Unrecognized option: ~a~n" (car args))
                (bail))))))))

  (define (usage opts #!optional (code -1))
    (print "Usage: scmus [options]")
    (print "Options:")
    (let loop ((opts opts))
      (unless (null? opts)
        (printf "    ~a" (car (opt-names (car opts))))
        (for-each (lambda (x) (display #\,) (display x))
                  (cdr (opt-names (car opts))))
        (for-each (lambda (x) (display #\space) (display x))
                  (opt-args (car opts)))
        (newline)
        (printf "        ~a~n" (opt-doc (car opts)))
        (loop (cdr opts))))
    (exit code)))
