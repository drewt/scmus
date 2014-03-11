;; views
(bind! "1" 'common (lambda () (set-view! 'library)) #t)
(bind! "2" 'common (lambda () (set-view! 'queue))   #t)
(bind! "3" 'common (lambda () (set-view! 'status))  #t)
(bind! "4" 'common (lambda () (set-view! 'error))   #t)

;; hjkl
(bind! "j" 'common (lambda () (win-move!  1)) #t)
(bind! "k" 'common (lambda () (win-move! -1)) #t)
(bind! "h" 'common (lambda () (seek! -5))     #t)
(bind! "l" 'common (lambda () (seek!  5))     #t)

;; arrow keys
(bind! "down"  'common (lambda () (win-move!  1)) #t)
(bind! "up"    'common (lambda () (win-move! -1)) #t)
(bind! "left"  'common (lambda () (seek! -5))     #t)
(bind! "right" 'common (lambda () (seek!  5))     #t)

;; movement
(bind! "^U" 'common (lambda () (win-move! -50 #t)) #t)
(bind! "^D" 'common (lambda () (win-move!  50 #t)) #t)
(bind! "page_up"   'common (lambda () (win-move! -100 #t)) #t)
(bind! "page_down" 'common (lambda () (win-move!  100 #t)) #t)

;; player control
(bind! "z" 'common prev!  #t)
(bind! "x" 'common play!  #t)
(bind! "c" 'common pause! #t)
(bind! "v" 'common stop!  #t)
(bind! "b" 'common next!  #t)

;; single, repeat, random, consume
(bind! "S" 'common toggle-single!  #t)
(bind! "R" 'common toggle-repeat!  #t)
(bind! "r" 'common toggle-random!  #t)
(bind! "C" 'common toggle-consume! #t)

(bind! "<" 'common (lambda () (seek! -60)) #t)
(bind! ">" 'common (lambda () (seek!  60)) #t)

;; search
(bind! "n" 'common (lambda () (win-search-next!)) #t)
(bind! "N" 'common (lambda () (win-search-prev!)) #t)

;; queue management
(bind! "a" 'common win-add! #t)
(bind! "d" 'common win-remove! #t)
(bind! "D" 'common win-clear! #t)

(bind! "enter" 'common win-activate! #t)
(bind! "(" 'common (lambda () (push! "(")) #t)

;; library view
(bind! "h"     'library win-deactivate! #t)
(bind! "left"  'library win-deactivate! #t)
(bind! "l"     'library win-activate!   #t)
(bind! "right" 'library win-activate!   #t)
