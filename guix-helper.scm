;;; guix-helper.scm --- Auxiliary scheme file for guix.el package

;; Copyright © 2014 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is used to add Guix directories to path variables and to
;; load the main code.

;;; Code:

(use-modules (srfi srfi-26))

(define %guix-dir)

;; The code is taken from ‘guix’ executable script
(define (set-paths!)
  (define-syntax-rule (push! elt v) (set! v (cons elt v)))

  (let ((module-dir (%site-dir))
        (updates-dir (and=> (or (getenv "XDG_CONFIG_HOME")
                                (and=> (getenv "HOME")
                                       (cut string-append <> "/.config")))
                            (cut string-append <> "/guix/latest"))))
    (push! module-dir %load-compiled-path)
    (if (and updates-dir (file-exists? updates-dir))
        (begin
          (set! %guix-dir updates-dir)
          (push! updates-dir %load-path)
          (push! updates-dir %load-compiled-path))
        (set! %guix-dir module-dir))))

(set-paths!)

(load-from-path "guix-main")

