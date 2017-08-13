;;; profiles.scm --- Code related to Guix profiles

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

;;; Code:

(define-module (emacs-guix profiles)
  #:use-module (guix profiles)
  #:use-module (srfi srfi-1)
  #:export (search-paths))

(define search-path-environment-variables
  ;; It is not exported from (guix scripts package) module.
  (@@ (guix scripts package) search-path-environment-variables))

(define* (search-paths profiles #:key (type 'exact))
  "Return a list with 'search paths' environment variables for PROFILES."
  (let* ((manifests (map profile-manifest profiles))
         (entries   (append-map manifest-transitive-entries
                                manifests)))
    (search-path-environment-variables
     entries profiles (const #f) #:kind type)))

;;; profiles.scm ends here
