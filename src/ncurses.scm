;;;; ncurses.scm

(declare
  (fixnum-arithmetic)
  (disable-interrupts)
  (foreign-declare "#include <ncurses.h>"))

(module (drewt ncurses)

  (endwin
   initscr
   box
   copywin
   delwin
   addstr
   getbegyx
   getmaxyx
   getparyx
   getyx
   getsyx
   isendwin
   mvwin
   newpad
   pnoutrefresh
   prefresh
   subpad
   derwin
   newwin
   subwin
   overwrite
   overlay
   refresh
   wrefresh
   scr_dump
   scr_init
   scr_restore
   scr_set
   setsyx
   is_linetouched
   is_wintouched
   touchline
   touchwin
   untouchwin
   wtouchln
   leaveok
   move
   wmove
   mvcur
   doupdate
   refresh
   wnoutrefresh
   wrefresh
   addch
   mvaddch
   mvwaddch
   waddch
   addnstr
   addstr
   mvaddstr
   mvaddnstr
   mvwaddstr
   mvwaddnstr
   waddnstr
   waddstr
   clear
   erase
   wclear
   werase
   clearok
   idlok
   scrollok
   setscrreg
   wsetscrreg
   clrtobot
   wclrtobot
   clrtoeol
   wclrtoeol
   delch
   mvdelch
   mvwdelch
   wdelch
   getstr
   getnstr
   mvgetnstr
   mvgetstr
   wgetnstr
   mvwgetnstr
   mvwgetstr
   inch
   mvinch
   winch
   mvwinch
   insch
   mvinsch
   winsch
   mvwinsch
   deleteln
   wdeleteln
   echochar
   wechochar
   flushinp
   insertln
   winsertln
   keyname
   meta
   nodelay
   scrl
   scroll
   wscrl
   unctrl
   ungetch
   wgetch
   getch
   cbreak
   raw
   nocbreak
   noraw
   def_prog_mode
   def_shell_mode
   reset_prog_mode
   reset_shell_mode
   delay_output
   echo
   noecho
   halfdelay
   has_ic
   has_il
   longname
   nl
   nonl
   notimeout
   timeout
   wtimeout
   resetty
   savetty
   can_change_color
   COLOR_PAIR
   has_colors
   init_color
   init_pair
   pair_content
   PAIR_NUMBER
   start_color
   COLORS
   COLOR_PAIRS
   attron
   attroff
   attrset
   wattron
   wattroff
   wattrset
   beep
   curs_set
   flash
   intrflush
   keypad
   standout
   standend
   wstandout
   wstandend
   erasechar
   killchar
   stdscr
   curscr
   LINES
   COLS
   COLOR_BLACK
   COLOR_RED
   COLOR_GREEN
   COLOR_MAGENTA
   COLOR_YELLOW
   COLOR_BLUE
   COLOR_CYAN
   COLOR_WHITE
   A_NORMAL
   A_UNDERLINE
   A_REVERSE
   A_BLINK
   A_BOLD
   A_DIM
   A_ALTCHARSET
   A_INVIS
   A_ATTRIBUTES
   A_CHARTEXT
   A_COLOR
   A_STANDOUT
   A_PROTECT
   A_LEFT
   A_RIGHT
   A_LOW
   A_TOP
   A_VERTICAL
   ACS_ULCORNER
   ACS_LLCORNER
   ACS_URCORNER
   ACS_LRCORNER
   ACS_RTEE
   ACS_LTEE
   ACS_BTEE
   ACS_TTEE
   ACS_HLINE
   ACS_VLINE
   ACS_PLUS
   ACS_S1
   ACS_S9
   ACS_CKBOARD
   ACS_DEGREE
   ACS_DIAMOND
   ACS_PLMINUS
   ACS_BULLET
   ACS_LARROW
   ACS_RARROW
   ACS_DARROW
   ACS_UARROW
   ACS_LANTERN
   ACS_BLOCK
   KEY_CODE_YES
   KEY_MIN
   KEY_BREAK
   KEY_SRESET
   KEY_RESET
   KEY_DOWN
   KEY_UP
   KEY_LEFT
   KEY_RIGHT
   KEY_HOME
   KEY_BACKSPACE
   KEY_F0
   KEY_DL
   KEY_IL
   KEY_DC
   KEY_IC
   KEY_EIC
   KEY_CLEAR
   KEY_EOS
   KEY_EOL
   KEY_SF
   KEY_SR
   KEY_NPAGE
   KEY_PPAGE
   KEY_STAB
   KEY_CTAB
   KEY_CATAB
   KEY_ENTER
   KEY_PRINT
   KEY_LL
   KEY_A1
   KEY_A3
   KEY_B2
   KEY_C1
   KEY_C3
   KEY_BTAB
   KEY_BEG
   KEY_CANCEL
   KEY_CLOSE
   KEY_COMMAND
   KEY_COPY
   KEY_CREATE
   KEY_END
   KEY_EXIT
   KEY_FIND
   KEY_HELP
   KEY_MARK
   KEY_MESSAGE
   KEY_MOVE
   KEY_NEXT
   KEY_OPEN
   KEY_OPTIONS
   KEY_PREVIOUS
   KEY_REDO
   KEY_REFERENCE
   KEY_REFRESH
   KEY_REPLACE
   KEY_RESTART
   KEY_RESUME
   KEY_SAVE
   KEY_SBEG
   KEY_SCANCEL
   KEY_SCOMMAND
   KEY_SCOPY
   KEY_SCREATE
   KEY_SDC
   KEY_SDL
   KEY_SELECT
   KEY_SEND
   KEY_SEOL
   KEY_SEXIT
   KEY_SFIND
   KEY_SHELP
   KEY_SHOME
   KEY_SIC
   KEY_SLEFT
   KEY_SMESSAGE
   KEY_SMOVE
   KEY_SNEXT
   KEY_SOPTIONS
   KEY_SPREVIOUS
   KEY_SPRINT
   KEY_SREDO
   KEY_SREPLACE
   KEY_SRIGHT
   KEY_SRSUME
   KEY_SSAVE
   KEY_SSUSPEND
   KEY_SUNDO
   KEY_SUSPEND
   KEY_UNDO
   KEY_MOUSE
   KEY_RESIZE
   KEY_F
   ERR
   printw
   wprintw
   mvprintw
   mvwprintw
   border
   wborder
   hline
   whline
   vline
   wvline
   mvhline
   mvwhline
   mvvline
   mvwvline
   bkgd
   bkgdset
   getbkgd
   wbkgd
   wbkgdset
   wresize
   use_default_colors
   assume_default_colors

   get-char
   getmouse
   has_mouse
   mouse-event-id
   mouse-event-x
   mouse-event-y
   mouse-event-z
   mouse-event-bstate
   mousemask
   BUTTON1_PRESSED
   BUTTON1_RELEASED
   BUTTON1_CLICKED
   BUTTON1_DOUBLE_CLICKED
   BUTTON1_TRIPLE_CLICKED
   BUTTON2_PRESSED
   BUTTON2_RELEASED
   BUTTON2_CLICKED
   BUTTON2_DOUBLE_CLICKED
   BUTTON2_TRIPLE_CLICKED
   BUTTON3_PRESSED
   BUTTON3_RELEASED
   BUTTON3_CLICKED
   BUTTON3_DOUBLE_CLICKED
   BUTTON3_TRIPLE_CLICKED
   BUTTON4_PRESSED
   BUTTON4_RELEASED
   BUTTON4_CLICKED
   BUTTON4_DOUBLE_CLICKED
   BUTTON4_TRIPLE_CLICKED
   BUTTON_SHIFT
   BUTTON_CTRL
   BUTTON_ALT
   ALL_MOUSE_EVENTS
   REPORT_MOUSE_POSITION
   )

  (import (scheme))
  (import (bind))
  (import (chicken base))
  (import (chicken condition))
  (import (chicken foreign))
  (import (chicken format))
  (import (chicken gc))
  (import (chicken memory))
  (import (chicken module))
  (import-for-syntax (chicken format))
  (import-for-syntax (chicken string))

  (cond-expand
    (ncurses-mouse-v2
      (export BUTTON5_PRESSED
              BUTTON5_RELEASED
              BUTTON5_CLICKED
              BUTTON5_DOUBLE_CLICKED
              BUTTON5_TRIPLE_CLICKED))
    (else))

(bind-options export-constants: #t)

(define-foreign-variable OK int)
(define-foreign-variable ERR_ int "ERR")

(define ERR ERR_)

(define (check code)
  (when (eq? code ERR_)
    (signal 
     (make-composite-condition
      (make-property-condition 'exn 'message "curses error")
      (make-property-condition 'curses) ) ) ) )

(define (to-chtype x)
  (if (char? x)
      (char->integer x)
      x) )

(define-foreign-type ptr c-pointer)
(define-foreign-type err int #f check)
(define-foreign-type chtype int to-chtype integer->char)
(define-foreign-type rchtype int to-chtype)
(define-foreign-type win (c-pointer "WINDOW"))

(define-syntax def
  (er-macro-transformer
   (lambda (sexp r c)
     (let* ((rt-sexp (cadr sexp))
            (name-sexp (caddr sexp))
            (name-string (->string (strip-syntax name-sexp)))
            (ats-sexp (cdddr sexp))
            (%define (r 'define))
            (%foreign-lambda (r 'foreign-lambda)))
       `(,%define ,name-sexp
          (,%foreign-lambda ,rt-sexp ,name-string ,@ats-sexp))))))

(define-syntax defv
  (er-macro-transformer
   (lambda (sexp r c)
     (let* ((rt-sexp (cadr sexp))
            (name-sexp (caddr sexp))
            (name-string (->string (strip-syntax name-sexp)))
            (%tmp (r 'tmp))
            (%begin (r 'begin))
            (%define-foreign-variable (r 'define-foreign-variable))
            (%define (r 'define)))
       `(,%begin
         (,%define-foreign-variable ,%tmp ,rt-sexp ,name-string)
         (,%define (,name-sexp) ,%tmp))))))

(define-syntax defc
  (er-macro-transformer
   (lambda (sexp r c)
     (let* ((rt-sexp (cadr sexp))
            (name-sexp (caddr sexp))
            (name-string (->string (strip-syntax name-sexp)))
            (%tmp (r 'tmp))
            (%begin (r 'begin))
            (%define-foreign-variable (r 'define-foreign-variable))
            (%define (r 'define)))
       `(,%begin
         (,%define-foreign-variable ,%tmp ,rt-sexp ,name-string)
         (,%define ,name-sexp ,%tmp))))))

(def err endwin)
(def ptr initscr)
; newterm
(def err box ptr chtype chtype)
(def err copywin ptr ptr int int int int int int bool)
(def err delwin ptr)

(define-syntax getpos
  (er-macro-transformer
   (lambda (sexp r c)
     (let ((m-sexp (cadr sexp))
           (%define (r 'define))
           (%lambda (r 'lambda))
           (%let (r 'let))
           (%get (r 'get))
           (%foreign-lambda* (r 'foreign-lambda*))
           (%void (r 'void))
           (%pointer (r 'c-pointer))
           (%int (r 'int))
           (%sprintf (r 'sprintf))
           (%let-location (r 'let-location))
           (%location (r 'location))
           (%values (r 'values)))
       `(,%define ,m-sexp
          (,%let ([,%get (,%foreign-lambda*
                      ;; ISSUE: renaming the void return type doesn't seem to work.
                      ;; ,%void
                      void
                      ([(,%pointer ,%int) yp] [(,%pointer ,%int) xp])
                      ,(sprintf "int y, x; ~A(y, x); *yp = y; *xp = x;" (strip-syntax m-sexp)))])
            (,%lambda ()
              (,%let-location ([y ,%int] [x ,%int])
                            (,%get (,%location y) (,%location x))
                            (,%values y x)))))))))

(define-syntax wgetpos
  (er-macro-transformer
   (lambda (sexp r c)
     (let ((m-sexp (cadr sexp))
           (%define (r 'define))
           (%get (r 'get))
           (%void (r 'void))
           (%win (r 'win))
           (%pointer (r 'c-pointer))
           (%int (r 'int))
           (%lambda (r 'lambda))
           (%let-location (r 'let-location))
           (%location (r 'location))
           (%values (r 'values))
           (%foreign-lambda* (r 'foreign-lambda*))
           (%let (r 'let)))
       `(,%define ,m-sexp
          ;; ISSUE: renaming the void return type doesn't seem to work.
          ;; (,%let ([,%get (,%foreign-lambda* ,%void ([,%win w] [(,%pointer ,%int) yp] [(,%pointer ,%int) xp])
          (,%let ([,%get (,%foreign-lambda* void ([,%win w] [(,%pointer ,%int) yp] [(,%pointer ,%int) xp])
                     ,(sprintf "int y, x; ~A(w, y, x); *yp = y; *xp = x;" (strip-syntax m-sexp)))])
          (,%lambda (w)
            (,%let-location ([y ,%int] [x ,%int])
                            (,%get w (,%location y) (,%location x))
                            (,%values y x)))))))))

(wgetpos getbegyx)
(wgetpos getmaxyx)
(wgetpos getparyx)
(wgetpos getyx)

(getpos getsyx)

(def bool isendwin)
(def err mvwin win int int)
(def ptr newpad int int)
(def err pnoutrefresh win int int int int int int)
(def err prefresh win int int int int int int)
(def ptr subpad ptr int int int int)
(def win derwin win int int int int)
(def win newwin int int int int)
(def win subwin win int int int int)
(def err overwrite win win)
(def err overlay win win)
(def void refresh)
(def void wrefresh win)
(def err scr_dump c-string)
(def err scr_init c-string)
(def err scr_restore c-string)
(def err scr_set c-string)
(def void setsyx int int)
(def bool is_linetouched win int)
(def bool is_wintouched win)
(def err touchline win int int)
(def err touchwin win)
(def err untouchwin win)
(def err wtouchln win int int bool)
(def void leaveok win bool)
(def err move int int)
(def err wmove win int int)
(def err mvcur int int int int)
(def err doupdate)
(def err refresh)
(def err wnoutrefresh win)
(def err wrefresh win)
(def err addch chtype)
(def err mvaddch int int chtype)
(def err mvwaddch win int int chtype)
(def err waddch win chtype)
(def err addnstr c-string int)
(def err addstr c-string)
(def err mvaddstr int int c-string)
(def err mvaddnstr int int c-string int)
(def err mvwaddstr win int int c-string)
(def err mvwaddnstr win int int c-string int)
(def err waddnstr win c-string int)
(def err waddstr win c-string)
(def err clear)
(def err erase)
(def err wclear win)
(def err werase win)
(def err clearok win bool)
(def err idlok win bool)
(def err scrollok win bool)
(def err setscrreg int int)
(def err wsetscrreg win int int)
(def err clrtobot)
(def err wclrtobot win)
(def err clrtoeol)
(def err wclrtoeol win)
(def err delch)
(def err mvdelch int int)
(def void mvwdelch win int int)
(def void wdelch win)
(def err getstr c-pointer)
(def err getnstr c-pointer int)
(def err mvgetnstr int int c-pointer int)
(def err mvgetstr int int c-pointer)
(def err wgetnstr win c-pointer int)
(def err mvwgetnstr win int int c-pointer int)
(def err mvwgetstr win int int c-pointer)
(def chtype inch)
(def chtype mvinch int int)
(def chtype winch win)
(def chtype mvwinch win int int)
(def chtype insch chtype)
(def chtype mvinsch int int chtype)
(def chtype winsch win chtype)
(def chtype mvwinsch win int int chtype)
(def err deleteln)
(def err wdeleteln win)
(def err echochar chtype)
(def err wechochar win chtype)
(def void flushinp)
(def err insertln)
(def err winsertln win)
(def c-string keyname int)
(def err meta win bool)
(def err nodelay win bool)
; scanw, wscanw, mvscanw, mvwscanw
(def err scrl int)
(def err scroll win)
(def err wscrl win int)
(def c-string unctrl chtype)
(def void ungetch int)
(def int wgetch win)
(define (getch) (wgetch (stdscr)))
(def err cbreak)
(def err raw)
(def err nocbreak)
(def err noraw)
(def err def_prog_mode)
(def err def_shell_mode)
(def err reset_prog_mode)
(def err reset_shell_mode)
; del_curterm, restartterm, set_curterm, setupterm
(def err delay_output int)
(def err echo)
(def err noecho)
; garbagedlines
(def err halfdelay int)
(def bool has_ic)
(def bool has_il)
(def c-string longname)
(def err nl)
(def err nonl)
(def err notimeout win bool)
(def void timeout int)
(def void wtimeout win int)
;  tputs
(def err resetty)
(def err savetty)
; ripoffline
; tgetent, tgetflag, tgetnum, tgetstr, tgoto, tigetflag, tigetnum, tigetstr, tparm
(def bool can_change_color)
; color_content
(def int COLOR_PAIR int)
(def bool has_colors)
(def err init_color short short short short)
(def err init_pair short short short)
; bkgd
(def void bkgdset chtype)
(def void wbkgdset win chtype)
(def int bkgd chtype)
(def int wbkgd win chtype)
(def chtype getbkgd win)
; wresize
(def int wresize win int int)
; default_colors
(def err use_default_colors)
(def err assume_default_colors int int)

(define pair_content
  (let ([content (foreign-lambda err "pair_content" short (c-pointer short) (c-pointer short))])
    (lambda (c)
      (let-location ([f short] [b short])
	(content c (location f) (location b))
	(values f b) ) ) ) )

(def int PAIR_NUMBER int)
(def err start_color)

(defv int COLORS)
(defv int COLOR_PAIRS)

(def err attron int)
(def err attroff int)
(def err attrset int)
(def err wattron win int)
(def err wattroff win int)
(def err wattrset win int)
(def void beep)
(def err curs_set int)
(def void flash)
(def err intrflush win bool)
(def err keypad win bool)
(def void standout)
(def void standend)
(def void wstandout win)
(def void wstandend win)
; typeahead
; vidattr, vid_attr, vidputs, vid_puts
; slk_attroff, slk_attr_off, slk_attron, slk_attr_on, slk_attrset, slk_attr_set, slk_clear,, slk_color, slk_init, slk_label
; slk_noutrefresh, slk_refresh, slk_restore, slk_set, slk_touch, slk_wset
; baudrate
(def char erasechar)
(def char killchar)
; erasewchar, killwchar
; filter
(defv win stdscr)
(defv win curscr)
(defv int LINES)
(defv int COLS)

(defc int COLOR_BLACK)
(defc int COLOR_RED)
(defc int COLOR_GREEN)
(defc int COLOR_MAGENTA)
(defc int COLOR_YELLOW)
(defc int COLOR_BLUE)
(defc int COLOR_CYAN)
(defc int COLOR_WHITE)

(defc int A_NORMAL)
(defc int A_UNDERLINE)
(defc int A_REVERSE)
(defc int A_BLINK)
(defc int A_BOLD)
(defc int A_DIM)
(defc int A_ALTCHARSET)
(defc int A_INVIS)
(defc int A_ATTRIBUTES)
(defc int A_CHARTEXT)
(defc int A_COLOR)
(defc int A_STANDOUT)
(defc int A_PROTECT)
(defc int A_LEFT)
(defc int A_RIGHT)
(defc int A_LOW)
(defc int A_TOP)
(defc int A_VERTICAL)

(defv rchtype ACS_ULCORNER)
(defv rchtype ACS_LLCORNER)
(defv rchtype ACS_URCORNER)
(defv rchtype ACS_LRCORNER)
(defv rchtype ACS_RTEE)
(defv rchtype ACS_LTEE)
(defv rchtype ACS_BTEE)
(defv rchtype ACS_TTEE)
(defv rchtype ACS_HLINE)
(defv rchtype ACS_VLINE)
(defv rchtype ACS_PLUS)

(defv rchtype ACS_S1)
(defv rchtype ACS_S9)
(defv rchtype ACS_CKBOARD)
(defv rchtype ACS_DEGREE)
(defv rchtype ACS_DIAMOND)
(defv rchtype ACS_PLMINUS)
(defv rchtype ACS_BULLET)
(defv rchtype ACS_LARROW)
(defv rchtype ACS_RARROW)
(defv rchtype ACS_DARROW)
(defv rchtype ACS_UARROW)
(defv rchtype ACS_LANTERN)
(defv rchtype ACS_BLOCK)

(bind #<<EOF
___declare(export_constants, yes)
#define KEY_CODE_YES	0400		/* A wchar_t contains a key code */
#define KEY_MIN		0401		/* Minimum curses key */
#define KEY_BREAK	0401		/* Break key (unreliable) */
#define KEY_SRESET	0530		/* Soft (partial) reset (unreliable) */
#define KEY_RESET	0531		/* Reset or hard reset (unreliable) */
/*
 * These definitions were generated by ./MKkey_defs.sh ./Caps
 */
#define KEY_DOWN	0402		/* down-arrow key */
#define KEY_UP		0403		/* up-arrow key */
#define KEY_LEFT	0404		/* left-arrow key */
#define KEY_RIGHT	0405		/* right-arrow key */
#define KEY_HOME	0406		/* home key */
#define KEY_BACKSPACE	0407		/* backspace key */
#define KEY_F0		0410		/* Function keys.  Space for 64 */
#define KEY_DL		0510		/* delete-line key */
#define KEY_IL		0511		/* insert-line key */
#define KEY_DC		0512		/* delete-character key */
#define KEY_IC		0513		/* insert-character key */
#define KEY_EIC		0514		/* sent by rmir or smir in insert mode */
#define KEY_CLEAR	0515		/* clear-screen or erase key */
#define KEY_EOS		0516		/* clear-to-end-of-screen key */
#define KEY_EOL		0517		/* clear-to-end-of-line key */
#define KEY_SF		0520		/* scroll-forward key */
#define KEY_SR		0521		/* scroll-backward key */
#define KEY_NPAGE	0522		/* next-page key */
#define KEY_PPAGE	0523		/* previous-page key */
#define KEY_STAB	0524		/* set-tab key */
#define KEY_CTAB	0525		/* clear-tab key */
#define KEY_CATAB	0526		/* clear-all-tabs key */
#define KEY_ENTER	0527		/* enter/send key */
#define KEY_PRINT	0532		/* print key */
#define KEY_LL		0533		/* lower-left key (home down) */
#define KEY_A1		0534		/* upper left of keypad */
#define KEY_A3		0535		/* upper right of keypad */
#define KEY_B2		0536		/* center of keypad */
#define KEY_C1		0537		/* lower left of keypad */
#define KEY_C3		0540		/* lower right of keypad */
#define KEY_BTAB	0541		/* back-tab key */
#define KEY_BEG		0542		/* begin key */
#define KEY_CANCEL	0543		/* cancel key */
#define KEY_CLOSE	0544		/* close key */
#define KEY_COMMAND	0545		/* command key */
#define KEY_COPY	0546		/* copy key */
#define KEY_CREATE	0547		/* create key */
#define KEY_END		0550		/* end key */
#define KEY_EXIT	0551		/* exit key */
#define KEY_FIND	0552		/* find key */
#define KEY_HELP	0553		/* help key */
#define KEY_MARK	0554		/* mark key */
#define KEY_MESSAGE	0555		/* message key */
#define KEY_MOVE	0556		/* move key */
#define KEY_NEXT	0557		/* next key */
#define KEY_OPEN	0560		/* open key */
#define KEY_OPTIONS	0561		/* options key */
#define KEY_PREVIOUS	0562		/* previous key */
#define KEY_REDO	0563		/* redo key */
#define KEY_REFERENCE	0564		/* reference key */
#define KEY_REFRESH	0565		/* refresh key */
#define KEY_REPLACE	0566		/* replace key */
#define KEY_RESTART	0567		/* restart key */
#define KEY_RESUME	0570		/* resume key */
#define KEY_SAVE	0571		/* save key */
#define KEY_SBEG	0572		/* shifted begin key */
#define KEY_SCANCEL	0573		/* shifted cancel key */
#define KEY_SCOMMAND	0574		/* shifted command key */
#define KEY_SCOPY	0575		/* shifted copy key */
#define KEY_SCREATE	0576		/* shifted create key */
#define KEY_SDC		0577		/* shifted delete-character key */
#define KEY_SDL		0600		/* shifted delete-line key */
#define KEY_SELECT	0601		/* select key */
#define KEY_SEND	0602		/* shifted end key */
#define KEY_SEOL	0603		/* shifted clear-to-end-of-line key */
#define KEY_SEXIT	0604		/* shifted exit key */
#define KEY_SFIND	0605		/* shifted find key */
#define KEY_SHELP	0606		/* shifted help key */
#define KEY_SHOME	0607		/* shifted home key */
#define KEY_SIC		0610		/* shifted insert-character key */
#define KEY_SLEFT	0611		/* shifted left-arrow key */
#define KEY_SMESSAGE	0612		/* shifted message key */
#define KEY_SMOVE	0613		/* shifted move key */
#define KEY_SNEXT	0614		/* shifted next key */
#define KEY_SOPTIONS	0615		/* shifted options key */
#define KEY_SPREVIOUS	0616		/* shifted previous key */
#define KEY_SPRINT	0617		/* shifted print key */
#define KEY_SREDO	0620		/* shifted redo key */
#define KEY_SREPLACE	0621		/* shifted replace key */
#define KEY_SRIGHT	0622		/* shifted right-arrow key */
#define KEY_SRSUME	0623		/* shifted resume key */
#define KEY_SSAVE	0624		/* shifted save key */
#define KEY_SSUSPEND	0625		/* shifted suspend key */
#define KEY_SUNDO	0626		/* shifted undo key */
#define KEY_SUSPEND	0627		/* suspend key */
#define KEY_UNDO	0630		/* undo key */
#define KEY_MOUSE	0631		/* Mouse event has occurred */
#define KEY_RESIZE	0632		/* Terminal resize event */
EOF
)

(define (KEY_F n) (+ KEY_F0 n))

(define (printw . args) (addstr (apply sprintf args)))
(define (wprintw w . args) (waddstr w (apply sprintf args)))
(define (mvprintw y x . args) (mvaddstr y x (apply sprintf args)))
(define (mvwprintw w y x . args) (mvwaddstr w y x (apply sprintf args)))

;;; Suggested by anonymous contributor:

(def err border chtype chtype chtype chtype chtype chtype chtype chtype)
(def err wborder win chtype chtype chtype chtype chtype chtype chtype chtype)
(def err hline chtype int)
(def err whline win chtype int)
(def err vline chtype int)
(def err wvline win chtype int)
(def err mvhline int int chtype int)
(def err mvwhline win int int chtype int)
(def err mvvline int int chtype int)
(def err mvwvline win int int chtype int)

(define (get-char)
  (cond-expand
    (wide-char
      (let ((*get-wch (foreign-lambda integer "get_wch" (c-pointer unsigned-int))))
        (let-location ((ch unsigned-int))
          (let ((rc (*get-wch (location ch))))
            (values ch rc)))))
    (else
      (let ((r (getch)))
        (cond
          ((= r ERR) (values 0 ERR))
          ((> r 255) (values r KEY_CODE_YES))
          (else      (values r OK)))))))

(define-foreign-type mouse-event c-pointer)

(define mouse-event-id
  (foreign-lambda* short ((mouse-event mev))
    "C_return(((MEVENT*)mev)->id);"))

(define mouse-event-x
  (foreign-lambda* int ((mouse-event mev))
    "C_return (((MEVENT*)mev)->x);"))

(define mouse-event-y
  (foreign-lambda* int ((mouse-event mev))
    "C_return (((MEVENT*)mev)->y);"))

(define mouse-event-z
  (foreign-lambda* int ((mouse-event mev))
    "C_return (((MEVENT*)mev)->z);"))

(define mouse-event-bstate
  (foreign-lambda* unsigned-long ((mouse-event mev))
    "C_return (((MEVENT*)mev)->bstate);"))

(define (getmouse)
  (let ((r ((foreign-lambda* mouse-event ()
              "MEVENT *mev = malloc(sizeof(MEVENT));"
              "getmouse(mev);"
              "C_return(mev);"))))
    (set-finalizer! r free)
    r))

(define has_mouse
  (foreign-lambda* bool () "return(has_mouse());"))

(define mousemask
  (foreign-lambda* unsigned-long ((unsigned-long newmask))
    "C_return(mousemask(newmask, NULL));"))

(define-syntax defkey
  (syntax-rules ()
    ((defkey name) (define name (foreign-value name integer)))))

(defkey BUTTON1_PRESSED)
(defkey BUTTON1_RELEASED)
(defkey BUTTON1_CLICKED)
(defkey BUTTON1_DOUBLE_CLICKED)
(defkey BUTTON1_TRIPLE_CLICKED)
(defkey BUTTON2_PRESSED)
(defkey BUTTON2_RELEASED)
(defkey BUTTON2_CLICKED)
(defkey BUTTON2_DOUBLE_CLICKED)
(defkey BUTTON2_TRIPLE_CLICKED)
(defkey BUTTON3_PRESSED)
(defkey BUTTON3_RELEASED)
(defkey BUTTON3_CLICKED)
(defkey BUTTON3_DOUBLE_CLICKED)
(defkey BUTTON3_TRIPLE_CLICKED)
(defkey BUTTON4_PRESSED)
(defkey BUTTON4_RELEASED)
(defkey BUTTON4_CLICKED)
(defkey BUTTON4_DOUBLE_CLICKED)
(defkey BUTTON4_TRIPLE_CLICKED)
(cond-expand
  (ncurses-mouse-v2
    (defkey BUTTON5_PRESSED)
    (defkey BUTTON5_RELEASED)
    (defkey BUTTON5_CLICKED)
    (defkey BUTTON5_DOUBLE_CLICKED)
    (defkey BUTTON5_TRIPLE_CLICKED))
  (else))
(defkey BUTTON_SHIFT)
(defkey BUTTON_CTRL)
(defkey BUTTON_ALT)
(defkey ALL_MOUSE_EVENTS)
(defkey REPORT_MOUSE_POSITION)
)
