;; gruvbox dark colorscheme
;; See https://github.com/morhertz/gruvbox

(let* ((palette '((red     124 #xcc241d) (*red    167 #xfb4934)
                  (green   106 #x98971a) (*green  142 #xb8bb26)
                  (yellow  172 #xd79921) (*yellow 214 #xfabd2f)
                  (blue     66 #x458588) (*blue   109 #x83a598)
                  (purple  132 #xb16286) (*purple 175 #xd3869b)
                  (aqua     72 #x689d6a) (*aqua   108 #x8ec07c)
                  (gray    246 #xa89984) (*gray   245 #x928374)
                  (bg0_h   234 #x1d2021)
                  (bg0     235 #x282828) (bg0_s   236 #x32302f)
                  (bg1     237 #x3c3836) (fg4     246 #xa89984)
                  (bg2     239 #x504945) (fg3     248 #xbdae93)
                  (bg3     241 #x665c54) (fg2     250 #xd5c4a1)
                  (bg4     243 #x7c6f64) (fg1     223 #xebdbb2)
                                         (fg0     229 #xfbf1c7)
                  (orange  166 #xd65d0e) (*orange 208 #xfe8019)))
       (? (lambda (name) (cadr (assoc name palette)))))
  (if (can-set-color?)
    (for-each (lambda (color)
                (set-color (cadr color) (caddr color)))
              palette))
  (set-option 'color-cmdline     `(default ,(? 'bg0)   ,(? 'fg1)))
  (set-option 'color-error       `(default ,(? 'bg0)   ,(? '*red)))
  (set-option 'color-info        `(default ,(? 'bg0)   ,(? '*yellow)))
  (set-option 'color-statusline  `(default ,(? 'bg1)   ,(? '*aqua)))
  (set-option 'color-titleline   `(default ,(? 'bg0_s) ,(? '*green)))
  (set-option 'color-win         `(default ,(? 'bg0)   ,(? 'fg1)))
  (set-option 'color-win-cur     `(default ,(? 'bg0)   ,(? '*purple)))
  (set-option 'color-win-cur-sel `(default ,(? 'bg1)   ,(? '*purple)))
  (set-option 'color-win-marked  `(default ,(? 'gray)  ,(? 'bg0)))
  (set-option 'color-win-sel     `(default ,(? 'bg1)   ,(? 'fg0)))
  (set-option 'color-win-title   `(default ,(? 'fg4)   ,(? 'bg0))))