#version=$(shell git rev-parse --verify --short HEAD 2>/dev/null)-git

AC_INIT(scmus, [m4_esyscmd_s([git rev-parse --verify --short HEAD 2>/dev/null])]-git)
AC_LANG(C)

# programs
AC_PROG_CC
AC_CHECK_PROG([HAVE_CSC], csc, yes, no)
if test $HAVE_CSC == no
then
	AC_MSG_ERROR([chicken compiler not found])
fi

# libraries
AC_CHECK_LIB(ncurses, getch)
AC_CHECK_LIB(ncursesw, get_wch)
if test "$ac_cv_lib_ncurses_getch" == no -a "$ac_cv_lib_ncursesw_get_wch" == no
then
	AC_MSG_ERROR([ncurses not found])
fi

if test "$ac_cv_lib_ncursesw_get_wch" == yes
then
	AC_SUBST(CONFIG_WIDE_CHAR, y)
	AC_SUBST(NCURSES_LIBS, -lncursesw)
else
	AC_MSG_WARN([configuring without wide character support])
	AC_SUBST(NCURSES_LIBS, -lncurses)
fi

AC_OUTPUT(Makefile)
