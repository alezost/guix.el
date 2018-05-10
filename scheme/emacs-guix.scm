;;; emacs-guix.scm --- Scheme side of Emacs-Guix

;; Copyright © 2016–2018 Alex Kost <alezost@gmail.com>

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

;; This module is loaded by the elisp side of Emacs-Guix right after
;; starting the *Guix REPL*.  It is used only to autoload all the
;; procedures from the other modules that can be called by the elisp
;; side.

;;; Code:

(define-module (emacs-guix)
  #:autoload (guix ui) (switch-to-generation*)
  #:autoload (emacs-guix commands) (guix-command
                                    guix-command-output
                                    help-string
                                    guix-output-to-file
                                    pipe-guix-output)
  #:autoload (emacs-guix licenses) (license-names
                                    lookup-license-uri
                                    license-sexps)
  #:autoload (emacs-guix packages) (profile->specifications+file-names
                                    package/output-sexps
                                    number-of-packages
                                    package-names*)
  #:autoload (emacs-guix generations) (generation-sexps)
  #:autoload (emacs-guix system-generations) (system-generation-sexps)
  #:autoload (emacs-guix locations) (package-location-string
                                     package-location-files
                                     package-location-sexps)
  #:autoload (emacs-guix services) (service-sexps)
  #:autoload (emacs-guix actions) (process-package-actions
                                   build-package*
                                   delete-generations*
                                   package-store-path
                                   package-source-file-name
                                   package-source-build-derivation
                                   package-build-log-file)
  #:autoload (emacs-guix graph) (graph-backend-names
                                 graph-node-type-names
                                 make-package-graph)
  #:autoload (emacs-guix hash) (file-hash)
  #:autoload (emacs-guix lint) (lint-checker-names
                                lint-package)
  #:autoload (emacs-guix pack) (compressor-names
                                pack-format-names)
  #:autoload (emacs-guix profiles) (search-paths)
  #:autoload (emacs-guix refresh) (refresh-updater-names)
  #:autoload (emacs-guix emacs) (%max-returned-list-size
                                 %temporary-directory
                                 to-emacs-side)
  #:autoload (emacs-guix utils) (search-load-path))

;;; emacs-guix.scm ends here
