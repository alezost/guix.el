# local.mk --- Local Automake file for emacs-lisp code  -*- makefile-automake -*-

# Copyright © 2014–2018 Alex Kost <alezost@gmail.com>
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

AM_V_ELC = $(AM_V_ELC_$(V))
AM_V_ELC_ = $(AM_V_ELC_$(AM_DEFAULT_VERBOSITY))
AM_V_ELC_0 = @echo "  ELC     " $@;

AM_ELCFLAGS = -L "$(abs_builddir)/%D%" -L "$(abs_srcdir)/%D%"

if GEISER_DIR
  AM_ELCFLAGS += -L "$(geiserlispdir)"
endif

if DASH_DIR
  AM_ELCFLAGS += -L "$(dashlispdir)"
endif

if BUI_DIR
  AM_ELCFLAGS += -L "$(builispdir)"
endif

if EDITINDIRECT_DIR
  AM_ELCFLAGS += -L "$(editindirectlispdir)"
endif

if POPUP_DIR
  AM_ELCFLAGS += -L "$(popuplispdir)"
endif

if EMACS_Q
  AM_ELCFLAGS += -Q
endif

AM_ELCFLAGS += $(shell test -v GUIX_ENVIRONMENT && echo --no-site-file)

AUTOLOADS = %D%/guix-autoloads.el

EL_FILES =					\
  %D%/guix.el					\
  %D%/guix-config.el				\
  %D%/guix-auto-mode.el				\
  %D%/guix-utils.el				\
  %D%/guix-external.el				\
  %D%/guix-profiles.el				\
  %D%/guix-scheme.el				\
  %D%/guix-guile.el				\
  %D%/guix-geiser.el				\
  %D%/guix-repl.el				\
  %D%/guix-help-vars.el				\
  %D%/guix-read.el				\
  %D%/guix-help.el				\
  %D%/guix-about.el				\
  %D%/guix-misc.el				\
  %D%/guix-build-log.el				\
  %D%/guix-command.el				\
  %D%/guix-env-var.el				\
  %D%/guix-devel.el				\
  %D%/guix-hydra.el				\
  %D%/guix-hydra-build.el			\
  %D%/guix-hydra-jobset.el			\
  %D%/guix-graph.el				\
  %D%/guix-hash.el				\
  %D%/guix-derivation.el			\
  %D%/guix-license.el				\
  %D%/guix-location.el				\
  %D%/guix-package.el				\
  %D%/guix-pcomplete.el				\
  %D%/guix-popup.el				\
  %D%/guix-prettify.el				\
  %D%/guix-ui-messages.el			\
  %D%/guix-ui.el				\
  %D%/guix-ui-license.el			\
  %D%/guix-ui-location.el			\
  %D%/guix-ui-profile.el			\
  %D%/guix-ui-package.el			\
  %D%/guix-ui-generation.el			\
  %D%/guix-ui-system-generation.el		\
  %D%/guix-ui-service.el

# Elisp files generated from ".in".
EL_GEN_FILES = %D%/guix-build-config.el

ELC_FILES = $(EL_GEN_FILES:%.el=%.elc) $(EL_FILES:%.el=%.elc)

dist_lisp_DATA = $(EL_FILES)

nodist_lisp_DATA =				\
  $(EL_GEN_FILES)				\
  $(ELC_FILES)					\
  $(AUTOLOADS)

$(AUTOLOADS): $(EL_FILES)
	$(AM_V_GEN) $(EMACS) -Q --batch --eval					\
	  "(let ((backup-inhibited t)						\
	         (generated-autoload-file \"$(abs_builddir)/$(AUTOLOADS)\"))	\
	     (update-directory-autoloads \"$(abs_srcdir)/%D%\"))"

$(ELC_FILES): %.elc: %.el
	-$(AM_V_ELC) $(EMACS) $(AM_ELCFLAGS) $(ELCFLAGS) --batch		\
	--load "$(abs_top_srcdir)/build-aux/build-env.el"			\
	-f batch-byte-compile $<

CLEANFILES += $(ELC_FILES) $(AUTOLOADS)

clean-lisp:
	-$(RM) -f $(ELC_FILES)

clean-elc: clean-lisp

.PHONY: clean-lisp clean-elc

# local.mk ends here
