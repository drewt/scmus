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

(module drewt.trie (empty-trie
                    trie?
                    trie->alist
                    trie->alist/prefix
                    trie-keys
                    trie-ref
                    trie-ref/prefix
                    trie-set!
                    trie-traverse)
  (import scheme
          (only chicken
                define-record-type
                let-optionals*
                optional)
          (only data-structures
                alist-ref
                alist-update!))

  ;; XXX: allocate a pair to use as a unique null tag (for EQ?)
  (define null-tag (cons 'null '()))

  (define-record-type trie
    (make-trie-node value children)
    trie?
    (value    node-value    node-value-set!)
    (children node-children node-children-set!))

  ;; Constructor for the empty trie.
  (define (empty-trie)
    (make-trie-node null-tag '()))

  ;; Returns #t if NODE does not have a value assigned.
  (define (node-empty? node)
    (eq? (node-value node) null-tag))

  ;; Returns #t if NODE is a leaf node.
  (define (node-is-leaf? node)
    (null? (node-children node)))

  ;; Add CHILD-NODE as the child under the key KEY in the trie NODE.
  ;; If there is already a child for KEY, it is replaced.
  (define (node-add-child! node key child-node)
    (node-children-set! node
      (alist-update! key child-node (node-children node))))

  ;; Get the child for KEY in the trie NODE.
  (define (child-ref key node)
    (alist-ref key (node-children node)))

  ;; Traverse the trie NODE until KEY is exhausted, and return the
  ;; sub-trie (i.e. the sub-trie prefixed by KEY).
  (define (trie-traverse node key #!optional default)
    (cond
      ((null? key)
        node)
      ((child-ref (car key) node) =>
        (lambda (child) (trie-traverse child (cdr key) default)))
      (else default)))

  ;; Look up KEY in the trie NODE.
  (define (trie-ref node key #!optional default)
    (let ((node (trie-traverse node key)))
      (if (and node (not (node-empty? node)))
        (node-value node)
        default)))

  ;; Set the value of KEY to VALUE in TRIE.
  (define (trie-set! trie key value)
    (let loop ((node trie) (key key))
      (cond
        ((null? key)
          (node-value-set! node value))
        ((child-ref (car key) node) =>
          (lambda (child) (loop child (cdr key))))
        (else
          (let ((fresh-node (empty-trie)))
            (node-add-child! node (car key) fresh-node)
            (loop fresh-node (cdr key)))))))

  ;; Convert the trie NODE to an association list.
  (define (trie->alist node)
    (define (*trie->alist key/node)
      (let* ((key   (car key/node))
             (node  (cdr key/node))
             (result (map (lambda (sub-key) (cons (cons key (car sub-key))
                                                  (cdr sub-key)))
                          (trie->alist node))))
        (if (node-empty? node)
          result
          (cons (cons (list key) (node-value node)) result))))
    (if (node-is-leaf? node)
      '()
      (apply append (map *trie->alist (node-children node)))))

  ;; Like TRIE->ALIST, but efficiently filters the result by a key-prefix.
  (define (trie->alist/prefix node prefix #!optional (default '()))
    (let ((unprefix (trie-traverse node prefix)))
      (if unprefix
        (map (lambda (k/v) (cons (append prefix (car k/v)) (cdr k/v)))
             (trie->alist unprefix))
        default)))

  ;; Like TRIE-REF, but returns successfully if PREFIX is an unambiguous
  ;; prefix for a value in the trie.  Also, unlike TRIE-REF, this function
  ;; returns the key as a second value.
  (define (trie-ref/prefix node prefix #!optional default)
    (let ((results (trie->alist/prefix node prefix)))
      (if (or (null? results)
              (not (null? (cdr results))))
        (values default #f)
        (values (cdar results) (caar results)))))

  (define (trie-keys node)
    (map car (trie->alist node)))

  (define (trie-values node)
    (map cdr (trie->alist node))))
