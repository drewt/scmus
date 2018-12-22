(require-extension srfi-1 utf8 utf8-srfi-13 utf8-srfi-14 vector-lib)

(import
  (except scheme
    string-length string-ref string-set! make-string string substring
    string->list list->string string-fill! write-char read-char display)
  (except chicken
    reverse-list->string print print*)
  (except data-structures
    conc string-chop string-split string-translate
    substring=? substring-ci=? substring-index substring-index-ci)
  (except extras
    read-string write-string read-token))

(define-constant CURSED-CMDLINE 1)
(define-constant CURSED-ERROR 2)
(define-constant CURSED-INFO 3)
(define-constant CURSED-STATUSLINE 4)
(define-constant CURSED-TITLELINE 5)
(define-constant CURSED-WIN 6)
(define-constant CURSED-WIN-CUR 7)
(define-constant CURSED-WIN-CUR-SEL 8)
(define-constant CURSED-WIN-SEL 9)
(define-constant CURSED-WIN-MARKED 10)
(define-constant CURSED-WIN-TITLE 11)
(define-constant NR-CURSED 11)
   
;; Macros

(define-syntax define-record/initform
  (syntax-rules (initialize)
    ((define-record/initform name ctor pred?
                             (slot initform . accessors) ...
                             (initialize (instance) fst . rest))
       (begin
         (define-record-type name (name slot ...) pred?
           (slot . accessors) ...)
         (define (ctor #!key (slot initform) ...)
           (let ((instance (name slot ...)))
             (begin fst . rest)
             instance))))
    ((define-record/initform name ctor pred? (slot initform . accessors) ...)
       (define-record/initform name ctor pred?
                               (slot initform . accessors) ...
                               (initialize (instance) (void))))))

(define-type format-spec list)
(define-type predicate (* -> boolean))
(define-type thunk (-> undefined))
(define-type track (list-of (pair symbol *)))

(define-type editable (struct editable))
(define-type mpd-connection (struct mpd-connection))
(define-type option (struct option))
(define-type frame (struct frame))
(define-type window (struct window))
(define-type widget (struct widget))

(define-type option-getter (option -> *))
(define-type option-setter (option * -> undefined))
