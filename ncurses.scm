;;;; ncurses.scm

(declare (fixnum)
         (disable-interrupts)
         (hide check to-chtype)
         (foreign-declare "#include <ncurses.h>"))

(module drewt.ncurses
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

   bkgdset
   getbkgd
   putp
   use_default_colors
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
   BUTTON5_PRESSED
   BUTTON5_RELEASED
   BUTTON5_CLICKED
   BUTTON5_DOUBLE_CLICKED
   BUTTON5_TRIPLE_CLICKED
   BUTTON_SHIFT
   BUTTON_CTRL
   BUTTON_ALT
   ALL_MOUSE_EVENTS
   REPORT_MOUSE_POSITION)

   (import scheme)
   (import chicken)
   (import foreign)
   (import lolevel)
   (import extras)
   (import easyffi)

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
    (lambda (sexp r c)
      (let* ((rt-sexp (cadr sexp))
             (name-sexp (caddr sexp))
             (name-string (->string (strip-syntax name-sexp)))
             (ats-sexp (cdddr sexp))
             (%define (r 'define))
             (%foreign-lambda (r 'foreign-lambda)))
        `(,%define ,name-sexp
           (,%foreign-lambda ,rt-sexp ,name-string ,@ats-sexp)))))

  (define-syntax defv
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
           (,%define (,name-sexp) ,%tmp)))))

  (define-syntax defc
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
          (,%define ,name-sexp ,%tmp)))))

  (def err endwin)
  (def ptr initscr)
  ; newterm
  (def err box ptr chtype chtype)
  (def err copywin ptr ptr int int int int int int bool)
  (def err delwin ptr)

  (define-syntax getpos
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
                             (,%values y x))))))))

  (define-syntax wgetpos
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
                             (,%values y x))))))))

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
  (def chtype wgetch win)
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

  (define-syntax defkey
    (syntax-rules ()
      ((defkey name) (define name (foreign-value name integer)))))

  (defkey KEY_CODE_YES)
  (defkey KEY_MIN)
  (defkey KEY_BREAK)
  (defkey KEY_SRESET)
  (defkey KEY_RESET)
  (defkey KEY_DOWN)
  (defkey KEY_UP)
  (defkey KEY_LEFT)
  (defkey KEY_RIGHT)
  (defkey KEY_HOME)
  (defkey KEY_BACKSPACE)
  (defkey KEY_F0)
  (defkey KEY_DL)
  (defkey KEY_IL)
  (defkey KEY_DC)
  (defkey KEY_IC)
  (defkey KEY_EIC)
  (defkey KEY_CLEAR)
  (defkey KEY_EOS)
  (defkey KEY_EOL)
  (defkey KEY_SF)
  (defkey KEY_SR)
  (defkey KEY_NPAGE)
  (defkey KEY_PPAGE)
  (defkey KEY_STAB)
  (defkey KEY_CTAB)
  (defkey KEY_CATAB)
  (defkey KEY_ENTER)
  (defkey KEY_PRINT)
  (defkey KEY_LL)
  (defkey KEY_A1)
  (defkey KEY_A3)
  (defkey KEY_B2)
  (defkey KEY_C1)
  (defkey KEY_C3)
  (defkey KEY_BTAB)
  (defkey KEY_BEG)
  (defkey KEY_CANCEL)
  (defkey KEY_CLOSE)
  (defkey KEY_COMMAND)
  (defkey KEY_COPY)
  (defkey KEY_CREATE)
  (defkey KEY_END)
  (defkey KEY_EXIT)
  (defkey KEY_FIND)
  (defkey KEY_HELP)
  (defkey KEY_MARK)
  (defkey KEY_MESSAGE)
  (defkey KEY_MOVE)
  (defkey KEY_NEXT)
  (defkey KEY_OPEN)
  (defkey KEY_OPTIONS)
  (defkey KEY_PREVIOUS)
  (defkey KEY_REDO)
  (defkey KEY_REFERENCE)
  (defkey KEY_REFRESH)
  (defkey KEY_REPLACE)
  (defkey KEY_RESTART)
  (defkey KEY_RESUME)
  (defkey KEY_SAVE)
  (defkey KEY_SBEG)
  (defkey KEY_SCANCEL)
  (defkey KEY_SCOMMAND)
  (defkey KEY_SCOPY)
  (defkey KEY_SCREATE)
  (defkey KEY_SDC)
  (defkey KEY_SDL)
  (defkey KEY_SELECT)
  (defkey KEY_SEND)
  (defkey KEY_SEOL)
  (defkey KEY_SEXIT)
  (defkey KEY_SFIND)
  (defkey KEY_SHELP)
  (defkey KEY_SHOME)
  (defkey KEY_SIC)
  (defkey KEY_SLEFT)
  (defkey KEY_SMESSAGE)
  (defkey KEY_SMOVE)
  (defkey KEY_SNEXT)
  (defkey KEY_SOPTIONS)
  (defkey KEY_SPREVIOUS)
  (defkey KEY_SPRINT)
  (defkey KEY_SREDO)
  (defkey KEY_SREPLACE)
  (defkey KEY_SRIGHT)
  (defkey KEY_SRSUME)
  (defkey KEY_SSAVE)
  (defkey KEY_SSUSPEND)
  (defkey KEY_SUNDO)
  (defkey KEY_SUSPEND)
  (defkey KEY_UNDO)
  (defkey KEY_MOUSE)
  (defkey KEY_RESIZE)

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

  ;;; definitions missing from the ncurses egg
  (def void bkgdset chtype)

  (def chtype getbkgd win)

  (define putp
    (foreign-lambda* int ((c-string str)) "putp(str);"))

  (define use_default_colors
    (foreign-lambda* integer () "return(use_default_colors());"))

  (define (get-char)
    (cond-expand
      (wide-char
        (let ((*get-wch (foreign-lambda integer "get_wch" (c-pointer unsigned-int))))
          (let-location ((ch unsigned-int))
            (let ((rc (*get-wch (location ch))))
              (values ch rc)))))
      (else
        (let ((r (char->integer (getch))))
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
  (defkey BUTTON5_PRESSED)
  (defkey BUTTON5_RELEASED)
  (defkey BUTTON5_CLICKED)
  (defkey BUTTON5_DOUBLE_CLICKED)
  (defkey BUTTON5_TRIPLE_CLICKED)
  (defkey BUTTON_SHIFT)
  (defkey BUTTON_CTRL)
  (defkey BUTTON_ALT)
  (defkey ALL_MOUSE_EVENTS)
  (defkey REPORT_MOUSE_POSITION))
