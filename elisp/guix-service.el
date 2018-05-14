;;; guix-service.el --- Guix services  -*- lexical-binding: t -*-

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

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

;; This file provides a general code related to Guix services.

;;; Code:

(require 'guix-read)
(require 'guix-location)

;;;###autoload
(defun guix-find-service-location-file (file &optional directory)
  "Open service location FILE.
See `guix-find-location' for the meaning of DIRECTORY.

Interactively, prompt for the location FILE.  With prefix
argument, prompt for DIRECTORY as well."
  (interactive
   (list (guix-read-service-location-file)
         (guix-read-directory)))
  (guix-find-location file directory))

(provide 'guix-service)

;;; guix-service.el ends here
