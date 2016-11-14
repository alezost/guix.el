define(EMACS_GUIX_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2016 Alex Kost <alezost@gmail.com>

This file is part of Emacs-Guix.

Emacs-Guix is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Emacs-Guix is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.
]])

AC_INIT([Emacs-Guix], [0.2], [alezost@gmail.com], [emacs-guix],
        [https://github.com/alezost/guix.el])

AC_COPYRIGHT(EMACS_GUIX_CONFIGURE_COPYRIGHT)

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])

dnl ----------------------------------------------------------------
dnl Guile side

GUILE_PKG([2.2 2.0])
GUILE_PROGS([2.0.2])

AC_ARG_WITH([guile-site-dir],
  [AS_HELP_STRING([--with-guile-site-dir],
    [installation directory for Guile (*.scm) files])],
  [guilemoduledir="$withval"],
  [guilemoduledir="$datarootdir/guile/site/$GUILE_EFFECTIVE_VERSION"])
AC_SUBST([guilemoduledir])
AC_MSG_CHECKING([for the Guile module directory])
AC_MSG_RESULT([$guilemoduledir])

AC_ARG_WITH([guile-site-ccache-dir],
  [AS_HELP_STRING([--with-guile-site-ccache-dir],
    [installation directory for Guile compiled (*.go) files])],
  [guileccachedir="$withval"],
  [guileccachedir="$libdir/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"])
AC_SUBST([guileccachedir])
AC_MSG_CHECKING([for the Guile ccache directory])
AC_MSG_RESULT([$guileccachedir])

AC_ARG_WITH([guix-site-dir],
  [AS_HELP_STRING([--with-guix-site-dir],
    [directory with Guix modules (*.scm files)])],
  [guixmoduledir="$withval"],
  [guixmoduledir=""])
AC_SUBST([guixmoduledir])

AC_ARG_WITH([guix-site-ccache-dir],
  [AS_HELP_STRING([--with-guix-site-ccache-dir],
    [directory with Guix compiled (*.go) files])],
  [guixccachedir="$withval"],
  [guixccachedir=""])

dnl If ccache dir is not specified, set it to the module dir.
AS_IF([test "x$guixccachedir" = "x" -a \
            "x$guixmoduledir" != "x"]
      [guixccachedir="$guixmoduledir"])

AC_SUBST([guixccachedir])

AM_CONDITIONAL([GUIX_DIR], [test "x$guixmoduledir" != "x"])

dnl ----------------------------------------------------------------
dnl Emacs side

AM_PATH_LISPDIR

AC_ARG_WITH([geiser-lispdir],
  [AS_HELP_STRING([--with-geiser-lispdir],
    [directory with Geiser *.el files])],
  [geiserlispdir="$withval"],
  [geiserlispdir="no"])
AC_SUBST([geiserlispdir])

AC_ARG_WITH([popup-lispdir],
  [AS_HELP_STRING([--with-popup-lispdir],
    [directory with magit-popup.el file])],
  [popuplispdir="$withval"],
  [popuplispdir="no"])
AC_SUBST([popuplispdir])

AM_CONDITIONAL([GEISER_DIR], [test "x$geiserlispdir" != "xno"])
AM_CONDITIONAL([POPUP_DIR], [test "x$popuplispdir" != "xno"])

dnl If all elisp dependencies are specified, we can use "emacs -Q" for
dnl byte-compilation.  Otherwise, "emacs" will be used, and it will
dnl hopefully find these packages in its 'load-path'.
AM_CONDITIONAL([EMACS_Q],
  [test "x$geiserlispdir" != "xno" -a \
        "x$popuplispdir" != "xno"])

dnl ----------------------------------------------------------------

AC_CONFIG_FILES([Makefile
                 scheme/Makefile
                 elisp/guix-build-config.el])

AC_OUTPUT
