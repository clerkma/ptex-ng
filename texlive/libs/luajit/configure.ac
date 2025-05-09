dnl Process this file with autoconf to produce a configure script.
dnl
dnl   Copyright 2016-2019 Karl Berry <tex-live@tug.org>
dnl   Copyright 2014-2015 Peter Breitenlohner <tex-live@tug.org>
dnl
dnl   This file is free software; the copyright holder
dnl   gives unlimited permission to copy and/or distribute it,
dnl   with or without modifications, as long as this notice is preserved.
dnl
m4_include([version.ac])[] dnl define luajit_version
AC_INIT([luajit for TeX Live], luajit_version, [tex-k@tug.org])
AC_PREREQ([2.65])
AC_CONFIG_SRCDIR([LuaJIT-src/src/luajit_rolling.h])
AC_CONFIG_AUX_DIR([../../build-aux])
AC_CONFIG_MACRO_DIRS([../../m4 m4])

KPSE_BASIC([luajit])

KPSE_CANONICAL_HOST

AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_AS
AC_PROG_RANLIB
AC_PROG_LN_S

AC_SUBST([LUAJITVERSION], [luajit_version])
KPSE_LT_VERSION([luajit])

LT_INIT([win32-dll])

KPSE_DLL_NAME([texluajit], [texluajit])

AC_CHECK_SIZEOF([void *])
ac_configure_args="$ac_configure_args HOST_PTR_SIZE='$ac_cv_sizeof_void_p'"

AC_SEARCH_LIBS([pow], [m])
AC_SEARCH_LIBS([dlopen], [dl])

KPSE_LUAJIT_DEFINES
KPSE_LARGEFILE([LUAJIT_DEFINES])

AS_CASE([$host_os],
        [cygwin* | mingw* | msys*], [LJHOST='Windows'],
        [darwin*], [LJHOST='Darwin'],
        [solaris* | sunos*], [LJHOST='SunOS'],
        [linux*], [LJHOST='Linux'],
        [freebsd*], [LJHOST='FreeBSD'],
                  [LJHOST='Other'])
LJ_ARCH()
AS_CASE([$LJHOST],
        [Windows], [LJVM_MODE=peobj
                    LUAJIT_CFLAGS="$LUAJIT_CFLAGS -malign-double"],
        [Darwin | iOS], [LJVM_MODE=machasm],
                        [LJVM_MODE=elfasm])

dnl Original test, adapted.
dnl AS_IF([ echo 'extern void b(void);int a(void){b();return 0;}' | $CC -c -x c - -o tmpunwind.o && { grep -qa -e eh_frame -e __unwind_info tmpunwind.o || grep -qU -e eh_frame -e __unwind_info tmpunwind.o; }],
AS_IF([ echo 'extern void b(void);int a(void){b();return 0;}' | $CC -c -x c - -o tmpunwind.o && { sed -n /eh_frame/p tmpunwind.o >tmpunwind_test || sed -n /__unwind_info/p tmpunwind.o >>tmpunwind_test && test -s tmpunwind_test; }],
      [LUAJIT_CFLAGS="$LUAJIT_CFLAGS -DLUAJIT_UNWIND_EXTERNAL"
       rm tmpunwind.o tmpunwind_test],
       [rm  tmpunwind.o tmpunwind_test])
lj_save_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS -fno-stack-protector"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],[[]])],
                  [LUAJIT_CFLAGS="$LUAJIT_CFLAGS -fno-stack-protector"])
CFLAGS=$lj_save_CFLAGS
AM_CONDITIONAL([PEOBJ], [test "x$LJVM_MODE" = xpeobj])

AC_SUBST([LUAJIT_CFLAGS])
AC_SUBST([LJHOST])
AC_SUBST([LJVM_MODE])

dnl Extra switches
dnl We support openbsd x86_64 and i386,
dnl other platforms could require different switches.
dnl
AS_CASE([$host_os:$host_cpu],
        [*darwin*:x86_64], [LIBLUAJIT_LDEXTRA='-image_base 7fff04c4a000'])
AS_CASE([$host_os:$host_cpu:$CC],
        [*openbsd*:x86_64:clang* | *openbsd*:x86_64:cc], [LIBLUAJIT_LDEXTRA='-lc++abi -lpthread'],
        [*openbsd*:i386:clang* | *openbsd*:i386:cc],   [LIBLUAJIT_LDEXTRA='-lc++abi -lpthread'])
AC_SUBST([LIBLUAJIT_LDEXTRA])

AM_CONDITIONAL([build], [test "x$enable_build" != xno])

if test "x$enable_build" != xno || test -f config.force; then

AS_IF([test "x$lj_cpp" != xok],
      [AC_MSG_ERROR([Sorry, can not preprocess <lj_arch.h>])])

echo timestamp >config.force
fi

# Check log2, on some systems is still not defined
LUAJIT_HAS_NOT_LOG2=
AC_CHECK_DECLS([log2], [], [LUAJIT_HAS_NOT_LOG2=[-DLUAJIT_NO_LOG2]], [[#include <math.h>]])
AC_SUBST([LUAJIT_HAS_NOT_LOG2])




AC_SUBST([LUAJIT_TREE], [LuaJIT-src])

dnl Not used but avoids putting a zillion useless -D's on the command line.
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_FILES([Makefile include/Makefile texluajit.pc])

dnl Pass DASM_ARCH to the subdirectory native,
dnl which must be configured for the build system.
AC_CONFIG_COMMANDS_POST([ac_configure_args="$ac_configure_args DASM_ARCH=$DASM_ARCH"])
KPSE_NATIVE_SUBDIRS([native])

AC_OUTPUT
