;;; guix-derivation-mode.el --- Guix derivation mode

;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>

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

;; Major mode for viewing Guix derivations.

;;; Code:

(require 'guix-utils)

(defgroup guix-derivation-mode nil
  "Prettify Guix derivation."
  :prefix "guix-derivation-mode-"
  :group  'guix
  :group  'convenience)

(defcustom guix-derivation-mode-regexp-file
  (rx "/gnu/store/"
      ;; Hash-parts do not include "e", "o", "u" and "t".  See base32Chars
      ;; at <https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc>
      (= 32 (any "0-9" "a-d" "f-n" "p-s" "v-z"))
      (* (any "-_+." alnum)) ".drv")
  "Regexp matching Guix derivations."
  :type  'regexp
  :group 'guix-derivation-mode)

(define-button-type 'guix-derivation-mode-find-file
  'follow-link t
  'action      #'guix-derivation-mode-button)

(defun guix-derivation-mode-button (button)
  "Guix derivation BUTTON."
  (guix-find-file (buffer-substring (button-start button)
                                    (button-end   button))))

(defun guix-derivation-mode-make-buttons ()
  "Create Guix derivation buttons."
  (guix-while-search guix-derivation-mode-regexp-file
    (make-button (match-beginning 0)
                 (match-end       0)
                 :type            'guix-derivation-mode-find-file)))

(defvar guix-derivation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>")     'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    map))

;;;###autoload
(define-derived-mode guix-derivation-mode special-mode "Guix-Derivation"
  "Major mode for viewing Guix derivations.

\\{guix-derivation-mode-map}"
  (guix-derivation-mode-make-buttons)
  (setq auto-save-mode nil)
  (let ((inhibit-read-only t))
    (guix-pretty-print-buffer (current-buffer)))
  (set-buffer-modified-p nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.drv\\'" . guix-derivation-mode))

(provide 'guix-derivation-mode)

;;; guix-derivation-mode.el ends here
