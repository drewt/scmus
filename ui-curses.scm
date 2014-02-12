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

(require-extension ncurses)

(declare (uses scmus-client))

(define *version-text* "scmus 0.1\nCopyright (C) 2014 Drew Thoreson\n")
(define *help-text* "I'll write docs later, OK?\n")

(define (main)
  ;(update)
  ;(main)
  #f 
  )

(define (start-color)
  #f
  )

(define (update-colors)
  #f
  )

(define (init-curses)
  (initscr)
  (cbreak)
  (keypad (stdscr) #t)
  (halfdelay 5)
  (noecho)
  (start-color)
  (update-colors))

(define (init-all)
  (init-client "localhost" 6600)
  (init-curses))

(define (exit-all)
  (endwin))

(define (process-args args)
  (if (null? args)
    #t
    (begin
      (case (string->symbol (car args))
        ((-v --version) (begin (display *version-text*) (exit 0)))
        ((-h --help) (begin (display *help-text*) (exit 0)))
        (else (display (string-append "Unrecognized option: \"" (car args) "\"\n"))))
      (process-args (cdr args)))))

(process-args (command-line-arguments))
(init-all)
(main)
(exit-all)
