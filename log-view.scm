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
        scmus.event
        scmus.log
        scmus.tui
        scmus.view
        scmus.widgets)

(define-class <log-text> (<textual>)
  ((headline  accessor: log-text-headline)
   (body      initform: #f
              accessor: log-text-body)
   (expanded? initform: #f
              accessor: log-text-expanded?)))

(define (make-log-text entry)
  (make <log-text> 'headline (format "<~a> ~a" (log-entry-type entry)
                                               (log-entry-short-message entry))
                   'body     (and (log-entry-long-message entry)
                                  (map (lambda (str) (string-append "   " str))
                                       (string-split (log-entry-long-message entry) "\n")))))

(define-method (widget-size (w <log-text>) available-cols available-rows)
  (values available-cols
          (min available-rows
               (if (log-text-expanded? w)
                 (+ 1 (length (log-text-body w)))
                 1))))

(define-method ((setter log-text-expanded?) after: (w <log-text>) expanded?)
  (widget-damaged! (widget-parent w)))

(define-method (text-text (w <log-text>))
  (cond
    ((log-text-expanded? w)
      (cons (string-append " - " (log-text-headline w))
            (log-text-body w)))
    ((log-text-body w)
      (list (string-append " + " (log-text-headline w))))
    (else
      (list (string-append "   " (log-text-headline w))))))

(define-method (widget-activate (w <log-text>))
  (when (log-text-body w)
    (set! (log-text-expanded? w) (not (log-text-expanded? w)))))

(define-class <log-window> (<window>))

(define *log-window*
  (make <log-window> 'data (map make-log-text (log-read))
                     'cursed CURSED-WIN
                     'cursed-fun (win-cursed-fun)))

(define-event-handler (log-changed) ()
  (let ((entries (map make-log-text (log-read))))
    (set! (list-box-data *log-window*) entries)
    (widget-activate (last entries))))

(define-view log
  (make-frame 'body *log-window*
              'header (make-text " Log" 'cursed CURSED-WIN-TITLE)))
