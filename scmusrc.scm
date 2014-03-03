;; hjkl
(bind! "j" 'common (lambda () (win-move!  1)) #t)
(bind! "k" 'common (lambda () (win-move! -1)) #t)
(bind! "h" 'common (lambda () (seek! -1))     #t)
(bind! "l" 'common (lambda () (seek!  1))     #t)

;; arrow keys
(bind! "down"  'common (lambda () (win-move!  1)) #t)
(bind! "up"    'common (lambda () (win-move! -1)) #t)
(bind! "left"  'common (lambda () (seek! -1))     #t)
(bind! "right" 'common (lambda () (seek!  1))     #t)

;; player control
(bind! "z" 'common prev!  #t)
(bind! "x" 'common play!  #t)
(bind! "c" 'common pause! #t)
(bind! "v" 'common stop!  #t)
(bind! "b" 'common next!  #t)

(bind! "<" 'common (lambda () (seek! -60)) #t)
(bind! ">" 'common (lambda () (seek!  60)) #t)

(bind! "enter" 'common win-activate! #t)
(bind! "(" 'common (lambda () (push! "(")) #t)
