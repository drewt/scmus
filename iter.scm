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

(module drewt.iter *
  (import scheme chicken srfi-1)

  ;;
  ;; An iter is a circular, doubly linked list with a sentinel head element.
  ;;
  (define-type iter (struct iter))
  (: iter-prev (iter -> iter))
  (: iter-set-prev! (iter iter -> undefined))
  (: iter-next (iter -> iter))
  (: iter-set-next! (iter iter -> undefined))
  (: iter-data (iter -> *))
  (: iter-set-data! (iter * -> undefined))
  (define-record-type iter
    (*make-iter prev next data)
    iter?
    (prev iter-prev iter-set-prev!)
    (next iter-next iter-set-next!)
    (data iter-data iter-set-data!))

  ;; Sentinel value to identify the head of an iter.  gensym is used to prevent
  ;; false positives in iter-head?.
  (define iter-sentinel (gensym 'iter-head))
 
  ;;
  ;; The syntax for an iter literal is:
  ;;
  ;; <iter>       -> #,(iter <iter-datum> ...)
  ;; <iter-datum> -> #,(iter-cursor <datum>)
  ;;              :: <datum>
  ;;

  ;;
  ;; Writer
  ;;

  (define-record-printer (iter obj out)
    ; display the data at the cursor as #,(cursor <data>)
    (define (format-node x)
      (if (eq? x obj)
        (format "#,(iter-cursor ~s)" (iter-data x))
        (format "~s" (iter-data x))))
    (display (iter-fold-nodes
               (lambda (x acc)
                 (string-append acc " " (format-node x)))
               "#,(iter"
               (iter-head obj))
             out)
    (display ")" out))

  (define-record-type iter-cursor
    (iter-cursor data)
    iter-cursor?
    (data cursor-data))

  ;;
  ;; Constructors
  ;;

  (: empty-iter (-> iter))
  (define (empty-iter)
    (let ((iter (*make-iter #f #f iter-sentinel)))
      (iter-set-next! iter iter)
      (iter-set-prev! iter iter)
      iter))

  (: iter (#!rest * -> iter))
  (define (iter #!rest vals)
    (list->iter vals))

  (: make-iter (fixnum #!optional * -> iter))
  (define (make-iter len #!optional (data #f))
    (define iter (empty-iter))
    (let loop ((i len))
      (if (positive? i)
        (begin
          (iter-add-before! iter data)
          (loop (- i 1)))
        iter)))

  ;;
  ;; Predicates
  ;;

  (: iter-empty? (iter -> boolean))
  (define (iter-empty? iter)
    (eq? iter (iter-next iter)))

  (: iter-is-head? (iter -> boolean))
  (define (iter-is-head? iter)
    (eqv? (iter-data iter) iter-sentinel))

  (: iter-is-node? (iter -> boolean))
  (define (iter-is-node? iter)
    (not (iter-is-head? iter)))

  (: iter-head? (iter -> boolean))
  (define (iter-head? val)
    (and (iter? val) (iter-is-head? val)))

  (: iter-node? (iter -> boolean))
  (define (iter-node? val)
    (and (iter? val) (iter-is-node? val)))

  ;;
  ;; Traversal
  ;;

  ;; Get the iter head from an iter node.
  (: iter-head (iter -> iter))
  (define (iter-head iter)
    (if (iter-head? iter)
      iter
      (iter-head (iter-prev iter))))

  (: iter-first (iter -> iter))
  (define (iter-first iter)
    (iter-next (iter-head iter)))

  (: iter-last (iter -> iter))
  (define (iter-last iter)
    (iter-prev (iter-head iter)))

  (: iter-walk (iter fixnum -> iter))
  (define (iter-walk iter k)
    (cond
      ((= k 0) iter)
      ((> k 0) (iter-walk (iter-next iter) (- k 1)))
      ((< k 0) (iter-walk (iter-prev iter) (+ k 1)))))

  (: iter-ref (iter fixnum -> *))
  (define (iter-ref iter k)
    (iter-data (iter-walk iter k)))

  ;;
  ;; Misc
  ;;

  (: iter-length (iter -> fixnum))
  (define (iter-length iter)
    (iter-fold (lambda (unused count)
                 (+ 1 count))
               0
               iter))

  (: iter-equal? (iter iter -> boolean))
  (define (iter-equal? a b)
    (let loop ((iter-a (iter-next a)) (iter-b (iter-next b)))
      (define data-a (iter-data iter-a))
      (define data-b (iter-data iter-b))
      (cond
        ((and (eq? iter-a a) (eq? iter-b b))
          #t)
        ((or (eq? iter-a a) (eq? iter-b b))
          #f)
        ((and (iter? data-a) (iter? data-b))
          (and (iter-equal? data-a data-b)
               (loop (iter-next iter-a) (iter-next iter-b))))
        (else
          (and (equal? data-a data-b)
               (loop (iter-next iter-a) (iter-next iter-b)))))))

  ;;
  ;; Mutation
  ;;

  ;; Add an iter-node with @data between @prev and @next, which must be adjacent
  ;; iter nodes.
  (: *iter-add! (iter iter * -> iter))
  (define (*iter-add! prev next data)
    (let ((new (*make-iter prev next data)))
      (iter-set-prev! next new)
      (iter-set-next! prev new)
      new))

  ;; Add an iter-node with @data before @iter.
  (: iter-add-before! (iter * -> iter))
  (define (iter-add-before! iter data)
    (*iter-add! (iter-prev iter) iter data))

  ;; Add an iter-node with @data after @iter.
  (: iter-add-after! (iter * -> iter))
  (define (iter-add-after! iter data)
    (*iter-add! iter (iter-next iter) data))

  ;; Add an iter-node with @data after the head.
  (: iter-add-head! (iter * -> iter))
  (define (iter-add-head! iter data)
    (iter-add-after! (iter-head iter) data))

  ;; Add an iter-node with @data at the tail.
  (: iter-add-tail! (iter * -> iter))
  (define (iter-add-tail! iter data)
    (iter-add-before! (iter-head iter) data))

  ;; Removes the given iter node and returns the next node.
  (: iter-remove! (iter -> iter))
  (define (iter-remove! iter)
    (let ((next (iter-next iter))
          (prev (iter-prev iter)))
      (iter-set-prev! next prev)
      (iter-set-next! prev next)
      next))

  ;;
  ;; Conversion
  ;;

  (: list->iter (list -> iter))
  (define (list->iter lst)
    (let ((cursor (empty-iter)))
      (fold (lambda (x iter)
              (if (iter-cursor? x)
                (let ((next (iter-add-after! iter (cursor-data x))))
                  (set! cursor next)
                  next)
                (iter-add-after! iter x)))
            cursor
            lst)
      cursor))
 
  (: iter->list (iter -> list))
  (define (iter->list head)
    (reverse
      (let loop ((iter (iter-next head)) (result '()))
        (if (eq? iter head)
          result
          (loop (iter-next iter) (cons (iter-data iter) result))))))

  ;;
  ;; Higher order functions.
  ;;

  ;; "The fundamental list iterator"--now for iters.
  ;; Assumes that @head is an iter head.
  (: iter-fold ((* * -> *) * iter -> *))
  (define (iter-fold fun init head)
    (let loop ((iter (iter-next head)) (acc init))
      (if (eq? iter head)
        acc
        (loop (iter-next iter) (fun (iter-data iter) acc)))))

  ;; Like iter-fold, but passing the iter nodes rather than the stored values.
  ;; It is safe for the passed-in function to delete the iter node it is given.
  (: iter-fold-nodes ((iter * -> *) * iter -> *))
  (define (iter-fold-nodes fun init head)
    (let loop ((iter (iter-next head))
               (next (iter-next (iter-next head)))
               (acc init))
      (if (eq? iter head)
        acc
        (loop next (iter-next next) (fun iter acc)))))

  ;; Like iter-fold, but doesn't assume that the iter argument is an iter head.
  ;; This function will iterate over the entire iter, looping around and skipping
  ;; past the head if @first isn't the head.
  (: iter-fold-from ((* * -> *) * iter -> *))
  (define (iter-fold-from fun init first)
    (let loop ((iter first) (acc init) (did-first #f))
      (cond
        ((and (eq? iter first) did-first)
          acc)
        ((iter-is-head? iter)
          (loop (iter-next iter) acc #t))
        (else
          (loop (iter-next iter)
                (fun (iter-data iter) acc)
                #t)))))

  (: iter-for-each (procedure iter -> undefined))
  (define (iter-for-each fun head)
    (iter-fold (lambda (x unused)
                 (fun x))
               #f
               head)
    (void))

  (: iter-for-each-from (procedure iter -> undefined))
  (define (iter-for-each-from fun first)
    (iter-fold-from (lambda (x unused)
                      (fun x))
                    #f
                    first)
    (void))

  (: iter-for-each-node (procedure iter -> undefined))
  (define (iter-for-each-node fun head)
    (iter-fold-nodes (lambda (x unused)
                       (fun x))
                     #f
                     head))

  (: iter-map (procedure iter -> iter))
  (define (iter-map fun head)
    (define result (empty-iter))
    (iter-fold (lambda (x iter)
                 (iter-add-after! iter (fun x)))
               result
               head)
    result)

  (: iter-map! (procedure iter -> iter))
  (define (iter-map! fun head)
    (iter-fold-nodes (lambda (node acc)
                       (iter-set-data! node (fun (iter-data node)))
                       acc)
                     head
                     head))

  ;; Like iter-map, but doesn't assume that @first is an iter head.  Loops as in
  ;; iter-for-each-from.
  (: iter-map-from ((* -> *) iter -> iter))
  (define (iter-map-from fun first)
    (define result (empty-iter))
    (iter-fold-from (lambda (x iter)
                      (iter-add-after! iter (fun x)))
                    result
                    first)
    result)

  ;; Like iter-map, but returns a list instead of an iter.
  (: iter-map->list (procedure iter -> list))
  (define (iter-map->list fun head)
    (define result (cons 'dummy '()))
    (iter-fold (lambda (x lst)
                 (let ((next (cons (fun x) '())))
                   (set-cdr! lst next)
                   next))
               result
               head)
    (cdr result))

  (: iter-filter ((* -> boolean) iter -> iter))
  (define (iter-filter fun head)
    (iter-fold (lambda (x iter)
                 (when (fun x)
                   (iter-add-before! iter x))
                 iter)
               (iter)
               head))

  (: iter-filter! ((* -> boolean) iter -> iter))
  (define (iter-filter! fun head)
    (iter-fold-nodes (lambda (node acc)
                       (unless (fun (iter-data node))
                         (iter-remove! node))
                       acc)
                     head
                     head))

  (define-reader-ctor 'iter iter)
  (define-reader-ctor 'iter-cursor iter-cursor))
