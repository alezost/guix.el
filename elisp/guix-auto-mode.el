;;; guix-auto-mode.el --- Settings for `auto-mode-alist'

;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>

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

;; This file contains "autoloads" for `auto-mode-alist' and some
;; variables that are used by regexps.

;;; Code:

;;;###autoload
(defvar guix-store-directory "/gnu/store"
  "Name of the Guix store directory.
See Info node `(guix) The Store'.

This string is used in various regular expressions and it
shouldn't end with a trailing slash.")

;;;###autoload
(defvar guix-hash-regexp
  ;; Hash-parts do not include "e", "o", "u" and "t".  See base32Chars
  ;; at <https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc>
  (rx (= 32 (any "0-9" "a-d" "f-n" "p-s" "v-z")))
  "Regexp matching hash part of a store file name.")

;;;###autoload
(unless (rassq 'guix-build-log-mode auto-mode-alist)
  ;; The purpose of this ^^^ check is to avoid running this sexp twice:
  ;; if `auto-mode-alist' contains some guix mode, then most likely this
  ;; sexp has already been evaluated by "autoloads.el".
  (let ((chars-rx (rx (+ (any alnum "-_+.")))))
    (setq
     auto-mode-alist
     (append
      ;; Regexp for log files (usually placed in /var/log/guix/...)
      `((,(rx-to-string `(and "/guix/drvs/" (= 2 alnum) "/" (= 30 alnum)
                              "-" (regexp ,chars-rx) ".drv" string-end)
                        t)
         . guix-build-log-mode)
        (,(rx-to-string `(and ,guix-store-directory "/"
                              (regexp ,chars-rx) ".drv" string-end)
                        t)
         . guix-derivation-mode)
        (,(rx-to-string `(and "/etc/profile" string-end) t)
         . guix-env-var-mode)
        (,(rx-to-string
           `(and "/tmp/guix-build-" (regexp ,chars-rx)
                 ".drv-" (one-or-more digit) "/environment-variables"
                 string-end)
           t)
         . guix-env-var-mode)
        (,(rx-to-string
           `(and "/guix/profiles/system" (* (regexp ,chars-rx)) "/"
                 (or "boot" "parameters")
                 string-end)
           t)
         . guix-scheme-mode)
        (,(rx-to-string
           `(and ,guix-store-directory "/" (regexp ,guix-hash-regexp) "-"
                 (or "activate" "activate-service" "boot" "parameters"
                     "shepherd.conf"
                     (and "shepherd" (regexp ,chars-rx) ".scm")
                     (and (regexp ,chars-rx) "-guile-builder"))
                 string-end)
           t)
         . guix-scheme-mode))
      auto-mode-alist))))

(provide 'guix-auto-mode)

;;; guix-auto-mode.el ends here
