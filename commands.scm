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

(import scmus.base scmus.client scmus.command scmus.command-line scmus.ueval
        scmus.keys scmus.status scmus.tui)

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

(define (invalid-argument-error nr msg)
  (abort (make-property-condition 'invalid-argument
                                  'argno nr
                                  'message msg)))

(define (map-command name sym)
  (register-command! name
    (lambda args
      (user-eval/raw (cons sym args)))))

; TODO: this command should take arguments, or not exist at all.  The below is
;       equivalent to the win-add command.
;(map-command "add" 'win-add!)

;; bind [-f] <context> <keys> [command]
;; bind common a-b-c '(do-something 42)'
;; If COMMAND is ommitted, then the current binding is printed.
(define-command/flags ("bind" ((#\f forced)) context keys #!optional command)
  (let ((context (with-input-from-string context read))
        (keys    (string-split keys "-"))
        (command (if command (with-input-from-string command read) #f)))
    (unless (binding-keys-valid? keys)
      (invalid-argument-error 2 "Invalid keys"))
    (if command
      (begin
        (when forced
          (unbind! keys context))
        (make-binding! keys context command))
      (command-line-print-info! (format "~s" (get-binding-expression
                                               keys
                                               context))))))

;; unbind [-f] <context> <keys>
(define-command/flags ("unbind" ((#\f forced)) context keys)
  (let ((context (with-input-from-string context read))
        (keys    (string-split keys "-")))
    (unless (binding-keys-valid? keys)
      (invalid-argument-error 2 "Invalid keys"))
    (if (and (not (unbind! keys context))
             (not forced))
      (invalid-argument-error 1 "No binding for keys"))))

(define-command ("clear") (scmus-clear!))

(map-command "colorscheme" 'colorscheme!)

;; connect [host [port [pass]]]
;; connect
;; connect localhost
;; connect localhost 6601
;; connect /path/to/socket unix
;; connect localhost 6601 hunter2
(define-command ("connect" #!optional (host #f) (port #f) (pass #f))
  (cond ((not host) (connect!))
        ((not port) (connect! host))
        ((string-ci=? port "unix") (connect! host #f pass))
        ((string-ci=? port "default") (connect! host 'default pass))
        (else (connect! host (string->number port) pass))))

;; echo [args...]
(define-command ("echo" . args)
  (command-line-print-info! (fold (lambda (x a)
                                    (string-append a " " x))
                                  (car args)
                                  (cdr args))))

(map-command "load" 'playlist-load!)
(map-command "save" 'playlist-save!)

(map-command "next" 'next!)
(map-command "pause" 'pause!)
; This command as implemented below is superfluous.  It should begin playing
; at the beginning of the track, and optionally take a track number/id as an
; argument.
;(map-command "play" 'play!)
(map-command "prev" 'prev!)
(map-command "stop" 'stop!)

(define-command ("seek" arg)
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
          (else (invalid-argument-error 1 "Invalid time format"))))
  (if (or (char=? (string-ref arg 0) #\+)
          (char=? (string-ref arg 0) #\-))
    (scmus-seek-cur! (string-append (string (string-ref arg 0))
                                    (number->string (parse-time (string-drop arg 1)))))
    (scmus-seek-cur! (number->string (parse-time arg)))))

;; TODO: set command
;;
;; e.g. :set consume on       ; for setting consume/repeat/random/single
;;      :set [option] [value] ; for setting scmus options

(define-command ("set" option value)
  (if (member option '("consume" "random" "repeat" "single"))
    (let ((fun (case (string->symbol option)
                 ((consume) scmus-consume-set!)
                 ((random)  scmus-random-set!)
                 ((repeat)  scmus-repeat-set!)
                 ((single)  scmus-single-set!))))
      (if (member value '("on" "off"))
        (fun (string=? value "on"))
        (invalid-argument-error 2 "Invalid value for option")))
    ; TODO: scmus options
    (invalid-argument-error 1 "Invalid option for 'set'")))

(define-command ("update")
  (scmus-update!))

(define-command ("vol" arg)
  (define (parse-vol arg)
    (unless (string-every char-set:digit arg)
      (invalid-argument-error 1 "Invalid volume format"))
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

(define-command ("win-activate")   (window-activate! (current-window)))
(define-command ("win-deactivate") (window-deactivate! (current-window)))
(define-command ("win-add")        (view-add! (current-view)))
(define-command ("win-remove")     (view-remove! (current-view)))
(define-command ("win-clear")      (view-clear! (current-view)))

(define-command ("win-search" query) (window-search! (current-window) query))
(define-command ("win-search-next")  (window-search-next! (current-window)))
(define-command ("win-search-prev")  (window-search-prev! (current-window)))

(define-command/flags ("win-move" ((#\r relative)) n)
  (unless (param-is-integer? n)
    (invalid-argument-error (if relative 2 1) "Not a number"))
  (window-move-cursor! (current-window) (string->number n) relative))

(define-command ("win-top") (window-move-top! (current-window)))
(define-command ("win-bottom") (window-move-bottom! (current-window)))

(define-command/flags ("win-move-tracks" ((#\b before)))
  (view-move! (current-view) before))

