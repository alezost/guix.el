# Makefile.am --- GNU Automake file to build Emacs-Guix

# Copyright © 2016–2017 Alex Kost <alezost@gmail.com>

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

CLEANFILES =

EXTRA_DIST = build-aux/build-env.el

info_TEXINFOS = doc/emacs-guix.texi

imagedir = $(pkgdatadir)/images
dist_image_DATA = images/*.svg

include elisp/local.mk

SUBDIRS = scheme

# Build "dir" file to make Emacs-Guix entry appear in the info top dir.

doc/dir: doc/emacs-guix.info
	$(AM_V_GEN) install-info $< $@

# Prepare html version of the manual (for github pages).

# "--css-ref=/css/manual.css" can't be used because the styles are put
# into "https://alezost.github.io/guix.el/css/".  And since I don't want
# to hardcode "guix.el" in the css-ref, the relative refs are used.
TEXI2HTML = texi2any --info --html
HTML_DIR = manual

manual: doc/emacs-guix.texi
	test -d $(HTML_DIR) || mkdir $(HTML_DIR)
	$(TEXI2HTML) --css-ref="../../../css/manual.css" \
	  --output=$(HTML_DIR)/html_node $<
	$(TEXI2HTML) --css-ref="../../css/manual.css" \
	  --output=$(HTML_DIR)/emacs-guix.html --no-split $<

clean-go:
	@cd scheme; make clean-go

.PHONY: clean-go

# Makefile.am ends here
