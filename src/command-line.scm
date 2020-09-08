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

(module (scmus command-line)
    (command-line-widget
     command-line-print-info!
     command-line-print-error!
     command-line-clear!
     command-line-text
     command-line-text-set!
     command-line-cursor-pos-set!
     command-line-enter-mode
     command-line-get-char
     command-line-get-string
     command-line-confirm
     make-command-line-mode
     make-completion-engine)
  (import coops
          (drewt iter)
          (drewt ncurses)
          (scmus base)
          (scmus event)
          (scmus tui))

  (define current-command-line-mode (make-parameter #f))

  (define-record-type command-line-mode
    (%make-command-line-mode prefix callback completion finally handle-key password? history)
    command-line-mode?
    (prefix     command-line-mode-prefix     command-line-mode-prefix-set!)
    (callback   command-line-mode-callback   command-line-mode-callback-set!)
    (completion command-line-mode-completion command-line-mode-completion-set!)
    (finally    command-line-mode-finally    command-line-mode-finally-set!)
    (handle-key command-line-mode-handle-key command-line-mode-handle-key-set!) 
    (password?  command-line-mode-password?  command-line-mode-password?-set!)
    (history    command-line-mode-history    command-line-mode-history-set!))

  (define (make-command-line-mode prefix callback
                                  #!key (engine *default-completion-engine*)
                                        finally
                                        handle-key
                                        password?)
    (%make-command-line-mode prefix callback engine finally handle-key password? (iter)))

  ;; tab completion {{{

  (define-record-type completion-engine
    (%make-completion-engine tokenize generator context)
    completion-engine?
    (tokenize      completion-tokenize)
    (generator     completion-generator)
    (context       completion-context
                   completion-context-set!))

  (define-record-type completion-context
    (make-completion-context completions token prefix suffix)
    completion-context?
    (completions context-completions
                 context-completions-set!)
    (token       context-token)
    (prefix      context-prefix)
    (suffix      context-suffix))

  (define (make-completion-engine tokenize generator)
    (%make-completion-engine (if (char-set? tokenize)
                               (char-set->tokenize tokenize)
                               tokenize)
                             generator
                             #f))

  (define *default-completion-engine*
    (make-completion-engine
      char-set:empty
      (lambda (_) '())))

  ;; Get the current completion engine.
  (define (completion-engine)
    (assert (current-command-line-mode) "completion-engine")
    (command-line-mode-completion (current-command-line-mode)))

  ;; Generate a tokenize function from a character set defining the token characters.
  ;; A tokenize function is a function taking a string and returning a list of pairs:
  ;;   (START . END)
  ;; Where,
  ;;   START is the start index of the token (inclusive)
  ;;   END   is the end index of the token (exclusive)
  (define (char-set->tokenize charset)
    (lambda (str)
      (let ((len (string-length str)))
        (let loop ((index 0) (start 0) (tokens '()))
          (cond
            ((= index len)
              ; reached end of string: return tokens
              (reverse (cons (cons start index) tokens)))
            ((not (char-set-contains? charset (string-ref str index)))
              (if (= start index)
                ; skip consecutive non-token chars
                (loop (+ index 1) (+ index 1) tokens)
                ; reached the end of a token: add to result
                (loop (+ index 1) (+ index 1) (cons (cons start index) tokens))))
            ; skip consecutive token chars
            (else (loop (+ index 1) start tokens)))))))

  ;; Extract the selected token and generate completions.
  (define (completion-init! engine)
    ;; Returns a list of tokens (as strings) in reverse order,
    ;; with the selected token at the head.
    (define (process-tokens text tokens index)
      (let loop ((tokens tokens) (result '()))
        (cond
          ((null? tokens)
            result)
          ((< index (caar tokens))
            (cons "" result))
          ; cursor is on this token: add it at head
          ((<= index (cdar tokens))
            (cons (substring/shared text (caar tokens) (cdar tokens))
                  result))
          ; cursor is beyond this token: add it at head, then loop
          (else
            (loop (cdr tokens)
                  (cons (substring/shared text (caar tokens) (cdar tokens))
                        result))))))
    (let* ((text (text-input-get-text command-line-widget))
           (index (text-input-get-cursor-pos command-line-widget))
           (*tokens ((completion-tokenize engine) text))
           (tokens (process-tokens text *tokens index))
           (selected (or (find (lambda (x) (and (>= index (car x))
                                                (<= index (cdr x))))
                               *tokens)
                         (cons index index)))
           (completions ((completion-generator engine) tokens)))
      (if (null? completions)
        #f
        (begin
          (completion-context-set! engine
            (make-completion-context (list->iter completions)
                                     (substring text (car selected) (cdr selected))
                                     (substring text 0 (car selected))
                                     (substring text (cdr selected))))
          #t))))

  ;; Call FUN on the completions iter for ENGINE, then substitute the completion
  ;; at the cursor.
  (define (completion-mv! engine fun)
    (let* ((context (completion-context engine))
           (next (fun (context-completions context)))
           (token (if (iter-head? next)
                    (context-token context)
                    (iter-data next))))
      (context-completions-set! context next)
      (text-input-set-text! command-line-widget
        (string-append (context-prefix context)
                       token
                       (context-suffix context)))
      (text-input-set-cursor-pos! command-line-widget
        (+ (string-length (context-prefix context))
           (string-length token)))))

  ;; Substitute the next completion.
  (define (completion-next! #!optional (engine (completion-engine)))
    (when (or (completion-context engine)
              (completion-init! engine))
      (completion-mv! engine iter-next)))

  ;; Substitute the previous completion.
  (define (completion-prev! #!optional (engine (completion-engine)))
    (when (or (completion-context engine)
              (completion-init! engine))
      (completion-mv! engine iter-prev)))

  (define (completion-reset! #!optional (engine (completion-engine)))
    (completion-context-set! engine #f))

  ;; tab completion }}}
  ;; history {{{

  (define history
    (getter-with-setter
      (lambda ()
        (assert (current-command-line-mode) "history")
        (command-line-mode-history (current-command-line-mode)))
      (lambda (x)
        (assert (current-command-line-mode) "(setter history")
        (command-line-mode-history-set! (current-command-line-mode) x))))

  (define (history-next!)
    (set! (history) (iter-next (history))))

  (define (history-prev!)
    (set! (history) (iter-prev (history))))

  (define (history-add! elm)
    (unless (string=? elm "")
      (iter-add-head! (history) elm)))

  (define (history-data)
    (let ((iter (history)))
      (if (iter-head? iter)
        ""
        (iter-data iter))))

  (define (history-reset!)
    (set! (history) (iter-head (history))))

  ;; history }}}

  (define-class <command-line> (<text-input>))

  (define-method (handle-input (widget <command-line>) input event)
    ; any key other than TAB/SHIFT+TAB resets completion context
    (unless (or (eqv? input #\tab)
                (eqv? input 353))
      (completion-reset!))
    (key-case input
      ((KEY_UP)
        (history-next!)
        (text-input-set-text! widget (history-data)))
      ((KEY_DOWN)
        (history-prev!)
        (text-input-set-text! widget (history-data)))
      ((KEY_BACKSPACE)
        (if (and (text-input-editing? widget)
                 (null? (text-input-text widget)))
          (text-input-cancel widget)
          (call-next-method)))
      ((#\tab)
        (completion-next!))
      ((353) ; shift+tab
        (completion-prev!))
      (else
        (unless (and (command-line-mode-handle-key (current-command-line-mode))
                     ((command-line-mode-handle-key (current-command-line-mode)) widget input))
          (call-next-method)))))

  (define-method (print-widget! (w <command-line>) x y cols rows)
    (if (and (current-command-line-mode)
             (command-line-mode-password? (current-command-line-mode)))
      (print-line! (string-append (text-input-prefix w)
                                  (make-string (length (text-input-text w)) #\*))
                   x y cols)
      (call-next-method)))

  (define command-line-widget
    (make <command-line>
      'cursed   CURSED-CMDLINE
      'prefix   " "))

  (define (command-line-on-leave)
    (history-reset!)
    (text-input-set-text! command-line-widget "")
    (set! (text-input-prefix command-line-widget) " ")
    (current-command-line-mode #f))

  (add-listener command-line-widget 'commit
    (lambda (s)
      (let ((then (command-line-mode-callback (current-command-line-mode)))
            (finally (command-line-mode-finally (current-command-line-mode))))
        (history-add! s)
        (command-line-on-leave)
        (then s)
        (when finally (finally)))))

  (add-listener command-line-widget 'cancel
    (lambda ()
      (let ((finally (command-line-mode-finally (current-command-line-mode))))
        (command-line-on-leave)
        (when finally (finally)))))

  (define (command-line-print-info! str)
    (unless (text-input-editing? command-line-widget)
      (set! (widget-cursed command-line-widget) CURSED-INFO)
      (text-input-set-text! command-line-widget str)))

  (define (command-line-print-error! str)
    (unless (text-input-editing? command-line-widget)
      (set! (widget-cursed command-line-widget) CURSED-ERROR)
      (text-input-set-text! command-line-widget str)))

  (define (command-line-clear!)
    (set! (text-input-text command-line-widget) '()))

  (define (command-line-text)
    (text-input-get-text command-line-widget))

  (define (command-line-text-set! str)
    (text-input-set-text! command-line-widget str))

  (define (command-line-cursor-pos-set! n)
    (text-input-set-cursor-pos! command-line-widget n))

  (define (command-line-enter-mode mode #!optional (text "") (cursor-pos 0))
    (current-command-line-mode mode)
    (set! (widget-cursed command-line-widget) CURSED-CMDLINE)
    (set! (text-input-prefix command-line-widget)
      (command-line-mode-prefix mode))
    (text-input-set-text! command-line-widget text)
    (text-input-set-cursor-pos! command-line-widget cursor-pos)
    (text-input-begin command-line-widget steal-focus: #t))

  (define (command-line-get-string prompt then . kwargs)
    (command-line-enter-mode
      (apply make-command-line-mode prompt then kwargs)))

  (define (command-line-get-char prompt then)
    (command-line-print-info! prompt)
    (get-key (lambda (k)
               (command-line-clear!)
               (then k))))

  (define (command-line-confirm question then)
    (command-line-get-char (string-append question " [y/N]")
      (lambda (c) (case c ((#\y #\Y) (then)))))))
