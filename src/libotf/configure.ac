dnl Autoconf script for libotf.

dnl Process this file with autoconf to produce a configure script.

dnl Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
dnl   National Institute of Advanced Industrial Science and Technology (AIST)
dnl   Registration Number H15PRO167

dnl This file is part of libotf.

dnl Libotf is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU Lesser General Public License as published
dnl by the Free Software Foundation; either version 2.1 of the License, or
dnl (at your option) any later version.

dnl Libotf is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl License for more details.

dnl You should have received a copy of the GNU Lesser General Public
dnl License along with this library, in a file named COPYING; if not,
dnl write to the Free Software Foundation, Inc., 59 Temple Place, Suite
dnl 330, Boston, MA 02111-1307, USA.

AC_INIT(libotf, 0.9.15, handa@m17n.org)
AM_INIT_AUTOMAKE(libotf, 0.9.15)
AM_CONFIG_HEADER(src/config.h)

# Checks for programs.
AC_PROG_CC
AM_PROG_LIBTOOL

# Checks for libraries.
AC_PATH_XTRA

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h])

AC_CHECK_HEADERS([X11/Xaw/Command.h], , no_x=yes)

AM_CONDITIONAL([X11_UNAVAILABLE], [test x$no_x = xyes])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MALLOC

# Check for Freetype2 usability.
AC_CHECK_PROG(HAVE_FREETYPE_CONFIG, freetype-config, yes)
if test "x$HAVE_FREETYPE_CONFIG" = "xyes"; then
  FREETYPE_INC=`freetype-config --cflags`
  CPPFLAGS="$CPPFLAGS $FREETYPE_INC"
  AC_CHECK_HEADER(ft2build.h, HAVE_FREETYPE=yes,
  			      HAVE_FREETYPE=no CPPFLAGS=$save_CPPFLAGS)
  if test "x$HAVE_FREETYPE" = "xyes" ; then
    FREETYPE_LD_FLAGS=`freetype-config --libs`;
    LIBS="$LIBS $FREETYPE_LD_FLAGS"
    AC_CHECK_LIB(freetype, FT_Init_FreeType, HAVE_FREETYPE=yes,
    			   		     HAVE_FREETYPE=no)
  fi
fi

if test "x$HAVE_FREETYPE" != "xyes" ; then
  echo "Freetype library wan't found in your system!"
  exit 1
fi
AC_SUBST(FREETYPE_INC)
AC_SUBST(FREETYPE_LD_FLAGS)

if test "x$no_x" != "xyes"; then
  X11_XT_XAW_XMU="-lX11 -lXt -lXaw -lXmu"
fi
AC_SUBST(X11_XT_XAW_XMU)

AC_CHECK_FUNC(scandir, HAVE_SCANDIR=yes, HAVE_SCANDIR=no)
if test "x$HAVE_SCANDIR" = "xyes"; then
  AC_DEFINE(HAVE_SCANDIR, 1,
	   [Define to 1 if you have scandir function.])
fi

AC_CHECK_FUNC(alphasort, HAVE_ALPHASORT=yes, HAVE_ALPHASORT=no)
if test "x$HAVE_ALPHASORT" = "xyes"; then
  AC_DEFINE(HAVE_ALPHASORT, 1,
	   [Define to 1 if you have alphasort function.])
fi

AC_CONFIG_FILES([Makefile src/Makefile example/Makefile
                 libotf-config libotf.pc])
AC_OUTPUT
