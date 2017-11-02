;;; guix-derivation.el --- Guix derivation mode

;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Copyright © 2017 Alex Kost <alezost@gmail.com>

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

;; This file provides `guix-derivation-mode', the major mode for Guix
;; derivation files (*.drv).

;;; Code:

(require 'guix-utils)

(defgroup guix-derivation nil
  "Settings for `guix-derivation-mode'."
  :prefix "guix-derivation-"
  :group  'guix)

(defgroup guix-derivation-faces nil
  "Faces for `guix-derivation-mode'."
  :group 'guix-derivation
  :group 'guix-faces)

(defface guix-derivation-file-name
  '((t :inherit link :underline nil))
  "Face for store file names."
  :group 'guix-derivation-faces)

(defcustom guix-derivation-file-regexp
  (rx-to-string `(and "/gnu/store/" (regexp ,guix-hash-regexp)
                      (+ (any "-_+." alnum)) ".drv")
                t)
  "Regexp matching Guix derivation file name."
  :type  'regexp
  :group 'guix-derivation)

(define-button-type 'guix-derivation-file
  'follow-link t
  'face        'guix-derivation-file-name
  'help-echo   "Visit this file"
  'action      #'guix-derivation-button)

(defun guix-derivation-button (button)
  "View file Guix derivation BUTTON."
  (guix-find-file (buffer-substring (button-start button)
                                    (button-end   button))))

(defun guix-derivation-make-buttons ()
  "Create buttons in the current Guix derivation buffer."
  (guix-while-search guix-derivation-file-regexp
    (make-text-button (match-beginning 0)
                      (match-end       0)
                      :type            'guix-derivation-file)))

(defvar guix-derivation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>")     'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    map)
  "Keymap for `guix-derivation-mode' buffers.")

;;;###autoload
(define-derived-mode guix-derivation-mode special-mode "Guix-Derivation"
  "Major mode for viewing Guix derivations.

\\{guix-derivation-mode-map}"
  ;; Set `font-lock-defaults' to make `global-guix-prettify-mode' work.
  (setq font-lock-defaults '(nil t))
  (let ((inhibit-read-only t))
    (guix-derivation-make-buttons)
    (guix-pretty-print-buffer (current-buffer)))
  (set-buffer-modified-p nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.drv\\'" . guix-derivation-mode))

(provide 'guix-derivation)

;;; guix-derivation.el ends here
