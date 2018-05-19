;;; screenshots.scm --- "Screenshots" page

;; Copyright © 2017–2018 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (site screenshots)
  #:use-module (ice-9 match)
  #:use-module (site shared)
  #:use-module (site utils)
  #:export (screenshots-page))

(define %images
  `(("about+help.png"
     "*Guix About* and *Guix Help* buffers"
     (,(emacs-command-tag "guix-about") " and "
      ,(emacs-command-tag "guix-help")))
    ("packages.png"
     "Packages"
     ,(manual-node-href "Packages"))
    ("generations.png"
     "Profile generations"
     ,(manual-node-href "Generations"
                        #:title "Profile generations"))
    ("system-generations.png"
     "System generations"
     "System generations")
    ("profiles.png"
     "Buffers with profiles"
     ,(manual-node-href "Profiles"))
    ("services.png"
     "Services"
     ,(manual-node-href "Services"))
    ("licenses.png"
     "List of licenses"
     ,(manual-node-href "Package-Licenses"
                        #:title "Package licenses"))
    ("locations.png"
     "Lists of locations"
     (,(manual-node-href "Package-Locations"
                        #:title "Package")
      " and "
      ,(manual-node-href "Services"
                        #:title "Service")
      " locations"))
    ("build-log+popup.png"
     "Guix Build Log mode and magit-like popup buffer"
     (,(manual-node-href "Build-Log-Mode"
                         #:title '(code "guix-build-log-mode"))
      " and "
      ,(manual-node-href "Popup-Interface"
                         #:title (emacs-command-tag "guix"))))
    ("hydra.png"
     "Buffers with the latest Hydra builds"
     ,(manual-node-href "Hydra"
                        #:title (emacs-command-tag
                                 "guix-hydra-latest-builds")))))

(define (image-div image-name alt description)
  `(div (@ (class "responsive"))
        (div (@ (class "gallery"))
             (a (@ (target "_blank")
                   (href ,(screenshot-url image-name)))
                (img (@ (src ,(screenshot-thumb-url image-name))
                        (alt ,alt)
                        (width "320") (height "180"))))
             (div (@ (class "desc")) ,description))))

(define (screenshots-page)
  "Return the screenshots page."
  `(html
    (@ (lang "en"))
    ,(head-tag "Screenshots" #:css (css-url "image-gallery.css"))
    (body
     ,(page-header)
     (h2 "Screenshots")
     ,@(map (match-lambda
              ((name alt desc)
               (image-div name alt desc)))
            %images)
     (div (@ (class "clearfix")))
     (p "Emacs theme used for these screenshots: "
        (a (@ (href "https://github.com/alezost/alect-themes"))
           "alect-light")
        "."))))

;;; screenshots.scm ends here
