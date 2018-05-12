# Makefile.am --- GNU Automake file to build guile modules

# Copyright © 2016–2018 Alex Kost <alezost@gmail.com>

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

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC     " $@;

MODULES =					\
  emacs-guix/utils.scm				\
  emacs-guix/emacs.scm				\
  emacs-guix/pack.scm				\
  emacs-guix/hash.scm				\
  emacs-guix/refresh.scm			\
  emacs-guix/commands.scm			\
  emacs-guix/licenses.scm			\
  emacs-guix/profiles.scm			\
  emacs-guix/packages.scm			\
  emacs-guix/generations.scm			\
  emacs-guix/system-generations.scm		\
  emacs-guix/graph.scm				\
  emacs-guix/lint.scm				\
  emacs-guix/services.scm			\
  emacs-guix/actions.scm			\
  emacs-guix.scm

GO_FILES = $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guileccache_DATA = $(GO_FILES)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>.
guile_install_go_files = install-nobase_nodist_guileccacheDATA
$(guile_install_go_files): install-nobase_dist_guilemoduleDATA

GUILEC_ENV =					\
  GUILE_AUTO_COMPILE=0

GUILEC_OPTS =					\
  -Warity-mismatch				\
  -Wformat					\
  -Wunbound-variable

if GUIX_DIR
  guix_module_part=":$(guixmoduledir)"
  guix_ccache_part=":$(guixccachedir)"
else
  guix_module_part=
  guix_ccache_part=
endif

# Guile PATHs shouldn't be unset: some guix modules want to load
# (gnutls) module, that's why 'guix' package propagates 'gnutls', i.e.
# a directory with gnutls module is placed in GUILE_LOAD_PATH.

GUILEC_ENV +=										   \
  GUILE_LOAD_PATH="$(abs_srcdir)$(guix_module_part):$$GUILE_LOAD_PATH"			   \
  GUILE_LOAD_COMPILED_PATH="$(abs_builddir)$(guix_ccache_part):$$GUILE_LOAD_COMPILED_PATH"

$(GO_FILES): %.go: %.scm
	-$(AM_V_GUILEC) $(GUILEC_ENV) \
	$(GUILD) compile $(GUILEC_OPTS) --output=$@ $<

CLEANFILES = $(GO_FILES)

clean-go:
	-$(RM) -f $(GO_FILES)

.PHONY: clean-go

# Makefile.am ends here
