# local.mk --- Local Automake file for emacs-lisp code  -*- makefile-automake -*-

# Copyright © 2014–2016 Alex Kost <alezost@gmail.com>
# Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>

# This file is part of Emacs-Guix.

# Emacs-Guix is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Emacs-Guix is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

AM_ELCFLAGS =

if GEISER_DIR
  AM_ELCFLAGS += -L "$(geiserlispdir)"
endif

if BUI_DIR
  AM_ELCFLAGS += -L "$(builispdir)"
endif

if POPUP_DIR
  AM_ELCFLAGS += -L "$(popuplispdir)"
endif

if EMACS_Q
  AM_ELCFLAGS += -Q
endif

AUTOLOADS = %D%/guix-autoloads.el

ELFILES =					\
  %D%/guix-about.el				\
  %D%/guix-base.el				\
  %D%/guix-build-log.el				\
  %D%/guix-buffer.el				\
  %D%/guix-command.el				\
  %D%/guix-config.el				\
  %D%/guix-devel.el				\
  %D%/guix-emacs.el				\
  %D%/guix-entry.el				\
  %D%/guix-external.el				\
  %D%/guix-geiser.el				\
  %D%/guix-guile.el				\
  %D%/guix-help-vars.el				\
  %D%/guix-history.el				\
  %D%/guix-hydra.el				\
  %D%/guix-hydra-build.el			\
  %D%/guix-hydra-jobset.el			\
  %D%/guix-info.el				\
  %D%/guix-init.el				\
  %D%/guix-license.el				\
  %D%/guix-list.el				\
  %D%/guix-location.el				\
  %D%/guix-messages.el				\
  %D%/guix-pcomplete.el				\
  %D%/guix-popup.el				\
  %D%/guix-prettify.el				\
  %D%/guix-profiles.el				\
  %D%/guix-read.el				\
  %D%/guix-repl.el				\
  %D%/guix-ui.el				\
  %D%/guix-ui-license.el			\
  %D%/guix-ui-location.el			\
  %D%/guix-ui-package.el			\
  %D%/guix-ui-generation.el			\
  %D%/guix-ui-system-generation.el		\
  %D%/guix-utils.el

dist_lisp_LISP = $(ELFILES)

nodist_lisp_LISP =				\
  %D%/guix-build-config.el			\
  $(AUTOLOADS)

$(AUTOLOADS): $(ELFILES)
	$(AM_V_GEN) $(EMACS) -Q --batch --eval						\
	  "(let ((backup-inhibited t)						\
	         (generated-autoload-file \"$(abs_builddir)/$(AUTOLOADS)\"))	\
	     (update-directory-autoloads \"$(abs_srcdir)/%D%\"))"

CLEANFILES += $(AUTOLOADS)

clean-elc: clean-lisp

.PHONY: clean-elc

# local.mk ends here
