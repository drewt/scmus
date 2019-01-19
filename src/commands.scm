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
        scmus.client
        scmus.command
        scmus.command-line
        scmus.config
        scmus.ueval
        scmus.keys
        scmus.status
        scmus.tui
        scmus.widgets)

(define (param-is-flags? p)
  (and (> (string-length p 1))
       (char=? (string-ref p 0) #\-)
       (not (char-numeric? (string-ref p 1)))))

(define (param-is-positive-integer? p)
  (string-every char-set:digit p))

(define (param-is-integer? p)
  (or (and (char=? (string-ref p 0) #\-)
           (param-is-positive-integer? (string-drop p 1)))
      (param-is-positive-integer? p)))

;; Define a command optionally accepting a number of character flags as the
;; first argument.  Flags arguments begin with the character '-', followed by
;; any number of characters.  E.g. "command -abc arg1 arg2".  Each character
;; following the "-" is interpreted as a separate flag.
(define-syntax define-command/flags
  (syntax-rules ()
    ((define-command/flags (cmd-name ((char flag-name) ...) . args) first . rest)
      (define-command (cmd-name . params)
        (let* ((has-flags  (and (not (null? params))
                                (param-is-flags? (car params))))
               (flag-chars (if has-flags
                             (cdr (string->list (car params)))
                             '()))
               (params     (if has-flags (cdr params) params))
               (flag-map   (map (lambda (ch) (not (not (member ch flag-chars))))
                                (list char ...))))
          (apply (lambda (flag-name ...)
                   (apply (lambda args first . rest) params))
                 flag-map))))))

(define (invalid-argument-error nr msg #!optional location)
  (raise (make-composite-condition
           (make-property-condition 'invalid-argument
                                    'argno nr
                                    'message msg)
           (make-property-condition 'exn
                                    'message "Invalid argument"
                                    'arguments '()
                                    'location location))))

(define (map-command name sym #!optional (completion '()))
  (register-command! name
    (lambda args
      (user-eval/raw (cons sym args)))
    completion))

; TODO: this command should take arguments, or not exist at all.  The below is
;       equivalent to the win-add command.
;(map-command 'add 'win-add!)

;; bind [-f] <context> <keys> [command]
;; bind common a-b-c '(do-something 42)'
;; If COMMAND is ommitted, then the current binding is printed.
(define-command/flags (bind ((#\f forced)) context keys #!optional command)
  (let ((context (with-input-from-string context read))
        (keys    (string-split keys " "))
        (cmd-str (and command (string-trim command))))
    ; validate arguments
    (unless (binding-keys-valid? keys)
      (invalid-argument-error 2 "Invalid keys" "bind")) 
    (when (and cmd-str (zero? (string-length cmd-str)))
      (invalid-argument-error 3 "Invalid binding expression" "bind"))
    (if (not command)
      ; no binding given: display current binding
      (command-line-print-info! (format "~s" (get-binding-expression keys context)))
      (let ((command (if (char=? (string-ref cmd-str 0) #\()
                       (with-input-from-string cmd-str read)
                       `(*command ,cmd-str))))
        (when forced
          (unbind! keys context))
        (make-binding! keys context command)))))
(register-command-completion! 'bind
  '(("common" "library" "queue" "search" "browser" "status" "error" "options" "bindings")))

;; unbind [-f] <context> <keys>
(define-command/flags (unbind ((#\f forced)) context keys)
  (let ((context (with-input-from-string context read))
        (keys    (string-split keys " ")))
    (unless (binding-keys-valid? keys)
      (invalid-argument-error 2 "Invalid keys" "unbind"))
    (if (and (not (unbind! keys context))
             (not forced))
      (invalid-argument-error 1 "No binding for keys" "unbind"))))
(register-command-completion! 'unbind
  '(("common" "library" "queue" "search" "browser" "status" "error" "options" "bindings")))

(define-command (clear) (scmus-clear!))

(map-command 'colorscheme 'colorscheme
  (list (lambda (tokens)
          (define (ls-schemes config-dir)
            (map (lambda (path)
                   (substring/shared path
                                     (+ (string-index-right path #\/) 1)
                                     (- (string-length path) 4)))
                 (if (file-exists? (format "~a/colors/" config-dir))
                   (glob (format "~a/colors/~a*.scm" config-dir (car tokens)))
                   '())))
          (append (ls-schemes *user-config-dir*)
                  (ls-schemes *scmus-dir*)))))

;; connect [host [port [pass]]]
;; connect
;; connect localhost
;; connect localhost 6601
;; connect /path/to/socket unix
;; connect localhost 6601 hunter2
(define-command (connect #!optional (host #f) (port #f) (pass #f))
  (cond ((not host) (connect!))
        ((not port) (connect! host))
        ((string-ci=? port "unix") (connect! host #f pass))
        ((string-ci=? port "default") (connect! host 'default pass))
        (else (connect! host (string->number port) pass))))

;; echo [args...]
(define-command (echo . args)
  (command-line-print-info! (fold (lambda (x a)
                                    (string-append a " " x))
                                  (car args)
                                  (cdr args))))

(let ((playlist-completion
        (lambda (tokens)
          (let ((playlists (map cdr (scmus-list-playlists))))
            (map (lambda (x) (string-append/shared "'" x "'"))
                 (filter (lambda (x) (string-prefix? (car tokens) x))
                         playlists))))))
  (map-command 'edit 'playlist-edit
    (list playlist-completion))
  (map-command 'load 'playlist-load
    (list playlist-completion))
  (map-command 'save 'playlist-save
    (list playlist-completion)))

(map-command 'next 'next)
(map-command 'pause 'pause)
(map-command 'play 'play)
(map-command 'prev 'prev)
(map-command 'quit 'quit)
(map-command 'stop 'stop)

(define-command (seek arg)
  ;; Parse ARG as a time string, returning a number of seconds.
  (define (parse-time arg)
    (define (*parse-time match h m s)
      (+ (* (if h (string->number (irregex-match-substring match h)) 0) 3600)
         (* (if m (string->number (irregex-match-substring match m)) 0) 60)
         (if s (string->number (irregex-match-substring match s)) 0)))
    (cond ((irregex-match "(\\d+):(\\d+):(\\d+)[s]?" arg) =>
            (lambda (m)
              (*parse-time m 1 2 3)))
          ((irregex-match "(\\d+):(\\d+)([ms]?)" arg) =>
            (lambda (m)
              (if (string=? (irregex-match-substring m 3) "m")
                (*parse-time m 1 2 #f)
                (*parse-time m #f 1 2))))
          ((irregex-match "(\\d+)([hms]?)" arg) =>
            (lambda (m)
              (let ((units (irregex-match-substring m 2)))
                (cond
                  ((string=? units "h") (*parse-time m 1 #f #f))
                  ((string=? units "m") (*parse-time m #f 1 #f))
                  (else (*parse-time m #f #f 1))))))
          (else (invalid-argument-error 1 "Invalid time format" "seek"))))
  (if (or (char=? (string-ref arg 0) #\+)
          (char=? (string-ref arg 0) #\-))
    (scmus-seek-cur! (string-append (string (string-ref arg 0))
                                    (number->string (parse-time (string-drop arg 1)))))
    (scmus-seek-cur! (number->string (parse-time arg)))))

;; TODO: set command
;;
;; e.g. :set consume on       ; for setting consume/repeat/random/single
;;      :set [option] [value] ; for setting scmus options

(define-command/completion (set (option '("consume" "random" "repeat" "single"))
                                (value  '("on" "off")))
  (if (member option '("consume" "random" "repeat" "single"))
    (let ((fun (case (string->symbol option)
                 ((consume) scmus-consume-set!)
                 ((random)  scmus-random-set!)
                 ((repeat)  scmus-repeat-set!)
                 ((single)  scmus-single-set!))))
      (if (member value '("on" "off"))
        (fun (string=? value "on"))
        (invalid-argument-error 2 "Invalid value for option" "set")))
    ; TODO: scmus options
    (invalid-argument-error 1 "Invalid option for 'set'" "set")))

(define-command (update)
  (scmus-update!))

(define-command (vol arg)
  (define (parse-vol arg)
    (unless (string-every char-set:digit arg)
      (invalid-argument-error 1 "Invalid volume format" "vol"))
    (string->number arg))
  (scmus-volume-set!
    (max 0
      (min 100
        (cond
          ((char=? (string-ref arg 0) #\+)
            (+ (scmus-volume) (parse-vol (string-drop arg 1))))
          ((char=? (string-ref arg 0) #\-)
            (- (scmus-volume) (parse-vol (string-drop arg 1))))
          (else (parse-vol arg)))))))

(map-command 'win-activate   'win-activate)
(map-command 'win-deactivate 'win-deactivate)
(define-command (win-add #!optional (dst "queue"))
  (user-eval/raw `(win-add (quote ,(string->symbol dst)))))
(map-command 'win-remove     'win-remove)
(map-command 'win-clear      'win-clear)

(map-command 'win-search 'win-search)
(map-command 'win-search-next 'win-search-next)
(map-command 'win-search-prev 'win-search-prev)

(define-command/flags (win-move ((#\r relative)) n)
  (unless (param-is-integer? n)
    (invalid-argument-error (if relative 2 1) "Not a number" "win-move"))
  (widget-move (widget-focus view-widget) (string->number n) relative))

(map-command 'win-top 'win-top)
(map-command 'win-bottom 'win-bottom)

(define-command/flags (win-move-tracks ((#\b before)))
  (widget-paste (widget-focus view-widget) before))

