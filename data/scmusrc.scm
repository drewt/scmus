(bind ":" 'common '(enter-command-mode) #t)
(bind "/" 'common '(enter-search-mode) #t)
(bind "$" 'common '(enter-eval-mode) #t)
(bind "q" 'common '(quit) #t)

;; views
(bind "1" 'common '(set-view 'library)  #t)
(bind "2" 'common '(set-view 'queue)    #t)
(bind "3" 'common '(set-view 'search)   #t)
(bind "4" 'common '(set-view 'browser)  #t)
(bind "5" 'common '(set-view 'status)   #t)
(bind "6" 'common '(set-view 'log)      #t)
(bind "7" 'common '(set-view 'options)  #t)
(bind "8" 'common '(set-view 'bindings) #t)

;; hjkl
(bind "j" 'common '(win-move  1) #t)
(bind "k" 'common '(win-move -1) #t)
(bind "h" 'common '(seek -5)     #t)
(bind "l" 'common '(seek  5)     #t)

;; arrow keys
(bind "down"  'common '(win-move  1) #t)
(bind "up"    'common '(win-move -1) #t)
(bind "left"  'common '(seek -5)     #t)
(bind "right" 'common '(seek  5)     #t)

;; movement
(bind "^U" 'common '(win-move -50 #t) #t)
(bind "^D" 'common '(win-move  50 #t) #t)
(bind "page_up"   'common '(win-move -100 #t) #t)
(bind "page_down" 'common '(win-move  100 #t) #t)
(bind "g" 'common '(win-top) #t)
(bind "G" 'common '(win-bottom) #t)

;; mouse
(bind "left_mouse_double_click" 'common '(win-activate) #t)
(bind "scroll_wheel_up"         'common '(win-move -3)  #t)
(bind "scroll_wheel_down"       'common '(win-move  3)  #t)

(bind "space" 'common '(begin (win-toggle-mark) (win-move 1)) #t)

(bind "i" 'common '(win-edit) #t)

;; player control
(bind "z" 'common '(prev)  #t)
(bind "x" 'common '(play)  #t)
(bind "c" 'common '(pause) #t)
(bind "v" 'common '(stop)  #t)
(bind "b" 'common '(next)  #t)

;; single, repeat, random, consume
(bind "S" 'common '(toggle-single)  #t)
(bind "R" 'common '(toggle-repeat)  #t)
(bind "r" 'common '(toggle-random)  #t)
(bind "C" 'common '(toggle-consume) #t)

(bind "<" 'common '(seek -60) #t)
(bind ">" 'common '(seek  60) #t)

;; search
(bind "n" 'common '(win-search-next) #t)
(bind "N" 'common '(win-search-prev) #t)

;; queue management
(bind "a" 'common '(win-add) #t)
(bind "d" 'common '(win-remove) #t)
(bind "D" 'common '(win-clear) #t)

(bind "enter" 'common '(win-activate) #t)

;; enter-eval-mode shortcuts
(bind "(" 'common '(enter-eval-mode "()" 1) #t)
(bind "!" 'common '(enter-eval-mode "(shell )" 8) #t)

;; library view
(bind "h"     'library '(win-deactivate) #t)
(bind "left"  'library '(win-deactivate) #t)
(bind "l"     'library '(win-activate)   #t)
(bind "right" 'library '(win-activate)   #t)

;; queue view
(bind "p" 'queue '(win-move-tracks))
(bind "P" 'queue '(win-move-tracks #t))

;; browser view
(bind "h"     'browser '(win-deactivate) #t)
(bind "left"  'browser '(win-deactivate) #t)
(bind "l"     'browser '(win-activate)   #t)
(bind "right" 'browser '(win-activate)   #t)
