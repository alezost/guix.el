# Makefile --- Makefile to generate html pages

# Copyright © 2017 Alex Kost <alezost@gmail.com>

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

EMACS_BATCH = emacs -Q --batch

HTML_FILES = index.html

all: $(HTML_FILES)

%.html: %.org
	$(EMACS_BATCH) $< \
	  --eval "(setq make-backup-files nil)" \
	  --funcall org-html-export-to-html

clean:
	rm -f $(HTML_FILES)

.PHONY: clean

# Makefile ends here
