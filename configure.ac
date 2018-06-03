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
AC_CHECK_LIB(tinfo, cbreak)

# determine which libs to use for ncurses
NCURSES_LIBS=""
if test "$ac_cv_lib_ncursesw_get_wch" = yes
then
	NCURSES_LIBS="-lncursesw"
elif test "$ac_cv_lib_ncurses_getch" = yes
then
	NCURSES_LIBS="-lncurses"
	AC_MSG_WARN([configuring without wide character support])
else
	AC_MSG_ERROR([ncurses not found])
fi

# sometimes we have some functions split off into libtinfo
if test "$ac_cv_lib_tinfo_cbreak" = yes
then
	NCURSES_LIBS+=" -ltinfo"
fi

AC_SUBST([NCURSES_LIBS])

AC_OUTPUT(Makefile)
