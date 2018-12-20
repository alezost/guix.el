;;; guix-read.el --- Minibuffer readers

;; Copyright © 2015–2018 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions to prompt a user for packages, system
;; types, hash formats and other guix related stuff.

;;; Code:

(require 'guix-help-vars)
(require 'guix-utils)
(require 'guix-repl)
(require 'guix nil t)


;;; Receivable lists of packages, lint checkers, etc.

(defcustom guix-package-names-use-duplicates t
  "Whether a list of package names is allowed to have duplicates or not.

These names are used as completions by several commands (for
example, by '\\[guix-packages-by-name]').

In short: `t' is fast but you'll see unwanted duplicates of
package names; `nil' is slow (only the first time) but there will
be no duplicates.  Read further for details.

To get a list of package names, Emacs-Guix walks through all of
the Guix packages.  And since Guix may have multiple packages
with the same name (like `gcc', `linux-libre', etc.), the default
list of all names contains duplicates (like 6 times of `gcc'
name).  Removing these duplicates from the list is a time
consuming operation, so it is not enabled by default, but you can
enable it by setting this variable to `nil'.

Note that Emacs-Guix caches the list of names when it is used the
first time, so the potential slowness will happen only during the
first call.  For the same reason (because of cashing) the new
value of this variable will take effect only after you restart
Emacs."
  :type 'boolean
  :group 'guix)

(guix-memoized-defun guix-system-types ()
  "Return a list of supported systems."
  (guix-eval-read "%supported-systems"))

(guix-memoized-defun guix-graph-backend-names ()
  "Return a list of names of available graph backends."
  (guix-eval-read "(graph-backend-names)"))

(guix-memoized-defun guix-graph-node-type-names ()
  "Return a list of names of available graph node types."
  (guix-eval-read "(graph-node-type-names)"))

(guix-memoized-defun guix-refresh-updater-names ()
  "Return a list of names of available refresh updater types."
  (guix-eval-read "(refresh-updater-names)"))

(guix-memoized-defun guix-lint-checker-names ()
  "Return a list of names of available lint checkers."
  (guix-eval-read "(lint-checker-names)"))

(guix-memoized-defun guix-compressor-names ()
  "Return a list of names of available compressors."
  (guix-eval-read "(compressor-names)"))

(guix-memoized-defun guix-pack-format-names ()
  "Return a list of names of available pack formats."
  (guix-eval-read "(pack-format-names)"))

(guix-memoized-defun guix-package-names ()
  "Return a list of names of available packages.
See also `guix-package-names-use-duplicates' variable."
  (let ((names (guix-eval-read "(package-names*)")))
    (sort (if guix-package-names-use-duplicates
              names
            (cl-delete-duplicates names :test #'string=))
          #'string<)))

(guix-memoized-defun guix-license-names ()
  "Return a list of names of available licenses."
  (guix-eval-read "(license-names)"))

(guix-memoized-defun guix-package-location-files ()
  "Return a list of available package locations."
  (sort (guix-eval-read "(package-location-files)")
        #'string<))

(guix-memoized-defun guix-service-names ()
  "Return a list of names of available services."
  (sort (guix-eval-read "(service-names*)")
        #'string<))

(guix-memoized-defun guix-service-location-files ()
  "Return a list of available service locations."
  (sort (guix-eval-read "(service-location-files)")
        #'string<))


;;; Readers

(defcustom guix-read-package-name-function #'guix-read-package-name-default
  "Function used to read a package name from minibuffer.
The function is called with 2 strings as arguments: a prompt and
initial-contents."
  :type '(choice (function-item guix-read-package-name-default)
                 (function-item guix-read-package-name-at-point)
                 (function :tag "Other function"))
  :group 'guix)

(defun guix-read-package-name-at-point (&optional prompt initial-contents)
  "Read symbol at point and if it is a package name, return it.
If it is not a package name or if current command has a prefix
argument, read the name from minibuffer."
  (if current-prefix-arg
      (guix-read-package-name-default prompt initial-contents)
    (let* ((at-point (thing-at-point 'symbol))
           (name     (and at-point
                          (substring-no-properties at-point))))
      (if (and name
               (member name (guix-package-names)))
          name
        (guix-read-package-name-default prompt
                                        initial-contents)))))

(defun guix-read-package-name (&optional prompt initial-contents)
  "Read a package name using `guix-read-package-name-function'."
  (funcall guix-read-package-name-function
           prompt initial-contents))

(guix-define-readers
 :completions-getter guix-system-types
 :single-reader guix-read-system-type
 :single-prompt "System type: ")

(guix-define-readers
 :completions-var guix-help-source-types
 :single-reader guix-read-source-type
 :single-prompt "Source type: ")

(guix-define-readers
 :completions-var guix-help-hash-formats
 :default guix-default-hash-format
 :single-reader guix-read-hash-format
 :single-prompt "Hash format: ")

(guix-define-readers
 :completions-var guix-help-refresh-subsets
 :single-reader guix-read-refresh-subset
 :single-prompt "Refresh subset: ")

(guix-define-readers
 :completions-getter guix-refresh-updater-names
 :multiple-reader guix-read-refresh-updater-names
 :multiple-prompt "Refresh updater,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-var guix-help-key-policies
 :default guix-default-key-policy
 :single-reader guix-read-key-policy
 :single-prompt "Key policy: ")

(guix-define-readers
 :completions-var guix-help-elpa-archives
 :default guix-default-elpa-archive
 :single-reader guix-read-elpa-archive
 :single-prompt "ELPA archive: ")

(guix-define-readers
 :completions-var guix-help-verify-options
 :multiple-reader guix-read-verify-options
 :multiple-prompt "Verify option,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-var guix-help-size-sort-keys
 :default guix-default-size-sort-key
 :single-reader guix-read-size-sort-key
 :single-prompt "Sort key: ")

(guix-define-readers
 :completions-var guix-help-search-paths-types
 :default guix-default-search-paths-type
 :single-reader guix-read-search-paths-type
 :single-prompt "Search paths type: ")

(guix-define-readers
 :completions-var guix-help-repl-types
 :default guix-default-repl-type
 :single-reader guix-read-repl-type
 :single-prompt "REPL type: ")

(guix-define-readers
 :completions-var guix-help-describe-formats
 :default guix-default-describe-format
 :single-reader guix-read-describe-format
 :single-prompt "'guix describe' format: ")

(guix-define-readers
 :completions-var guix-help-on-error-strategies
 :default guix-default-on-error-strategy
 :single-reader guix-read-on-error-strategy
 :single-prompt "On error strategy: ")

(guix-define-readers
 :completions-getter guix-graph-backend-names
 :default guix-default-graph-backend
 :single-reader guix-read-graph-backend
 :single-prompt "Graph backend: ")

(guix-define-readers
 :completions-getter guix-graph-node-type-names
 :default guix-default-graph-node-type
 :single-reader guix-read-graph-node-type
 :single-prompt "Graph node type: ")

(guix-define-readers
 :completions-getter guix-lint-checker-names
 :multiple-reader guix-read-lint-checker-names
 :multiple-prompt "Linter,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-getter guix-compressor-names
 :single-reader guix-read-compressor-name
 :single-prompt "Compressor: ")

(guix-define-readers
 :completions-getter guix-pack-format-names
 :single-reader guix-read-pack-format-name
 :single-prompt "Pack format: ")

(guix-define-readers
 :completions-getter guix-package-names
 :require-match nil
 :single-reader guix-read-package-name-default
 :single-prompt "Package: "
 :multiple-reader guix-read-package-names
 :multiple-prompt "Package,s: "
 :multiple-separator " ")

(guix-define-readers
 :completions-getter guix-license-names
 :single-reader guix-read-license-name
 :single-prompt "License: ")

(guix-define-readers
 :completions-getter guix-package-location-files
 :single-reader guix-read-package-location-file
 :single-prompt "Package location file: ")

(guix-define-readers
 :completions-getter guix-service-names
 :require-match nil
 :single-reader guix-read-service-name
 :single-prompt "Service: ")

(guix-define-readers
 :completions-getter guix-service-location-files
 :single-reader guix-read-service-location-file
 :single-prompt "Service location file: ")

(provide 'guix-read)

;;; guix-read.el ends here
