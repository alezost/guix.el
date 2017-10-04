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

RAW_HTML_DIR = html-raw
SCREENSHOTS_DIR = images/screenshots
THUMBS_DIR = $(SCREENSHOTS_DIR)/thumbs
SCREENSHOTS = $(notdir $(wildcard $(SCREENSHOTS_DIR)/*.png))
THUMBS_FILES = $(foreach name, $(SCREENSHOTS), $(THUMBS_DIR)/$(name))

all: html
thumbs: $(THUMBS_FILES)

$(THUMBS_DIR)/%.png: $(SCREENSHOTS_DIR)/%.png
	convert $< -thumbnail 320 $@

html:
	@./build-html.scm
	@find $(RAW_HTML_DIR) -name '*.html' | \
	  while read file_name; do \
	    base_name=`basename "$$file_name"`; \
	    echo "tidying page '$$base_name'"; \
	    tidy -quiet -indent -output "$$base_name" "$$file_name"; \
	  done

.PHONY: thumbs html

# Makefile ends here
