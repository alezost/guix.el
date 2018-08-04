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

(require 'cl-lib)
(require 'guix-auto-mode)  ; for `guix-store-directory'
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
  '((t :inherit bui-file-name))
  "Face for store file names."
  :group 'guix-derivation-faces)

(defface guix-derivation-drv-file-name
  '((default :inherit guix-derivation-file-name)
    (((class color) (background light)) :foreground "SpringGreen4")
    (((class color) (background dark)) :foreground "SpringGreen3"))
  "Face for '*.drv' store file names."
  :group 'guix-derivation-faces)

(defcustom guix-derivation-file-regexp
  (rx-to-string `(and ,guix-store-directory "/"
                      (+ (not (any "\" "))))
                t)
  "Regexp matching Guix derivation file name."
  :type  'regexp
  :group 'guix-derivation)

(defcustom guix-derivation-file-regexp-group 0
  "Regexp group in `guix-derivation-file-regexp'."
  :type 'integer
  :group 'guix-derivation)

(define-button-type 'guix-derivation-file
  'follow-link t
  'face        nil
  'help-echo   "Visit this file"
  'action      #'guix-derivation-button)

(defvar guix-derivation-file-name-faces
  '(("\\.drv\\'" . guix-derivation-drv-file-name)
    ("" . guix-derivation-file-name))
  "Alist used to define faces to highlight store file names.
Each element of the list has a form:

  (REGEXP . FACE)

If any substring of the file name matches REGEXP, this file name
will be highlighted with FACE.")

(defun guix-derivation-file-name-face (file-name)
  "Return a face to highlight FILE-NAME.
See `guix-derivation-file-name-faces'."
  (cdr (cl-find-if (lambda (assoc)
                     (string-match-p (car assoc) file-name))
                   guix-derivation-file-name-faces)))

(defun guix-derivation-button (button)
  "View file Guix derivation BUTTON."
  (guix-find-file (buffer-substring (button-start button)
                                    (button-end   button))))

(defun guix-derivation-make-buttons ()
  "Create buttons in the current Guix derivation buffer."
  (guix-while-search guix-derivation-file-regexp
    (let* ((beg    (match-beginning guix-derivation-file-regexp-group))
           (end    (match-end       guix-derivation-file-regexp-group))
           (string (substring-no-properties
                    (match-string guix-derivation-file-regexp-group)))
           (face   (guix-derivation-file-name-face string)))
      (apply #'make-text-button
             beg end
             :type 'guix-derivation-file
             (and face `(font-lock-face ,face))))))

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
    (guix-pretty-print-buffer)
    (guix-derivation-make-buttons))
  (set-buffer-modified-p nil))

(provide 'guix-derivation)

;;; guix-derivation.el ends here
