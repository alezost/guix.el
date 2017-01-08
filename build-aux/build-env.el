;; Set up Emacs to build *.el files.

(setq load-prefer-newer t)

;; When we are inside "guix environment", autoload Emacs packages from
;; the environment profile.  This makes elisp dependencies be found
;; during compilation, so it is not necessary to specify
;; '--with-XXX-lispdir' options to "configure" script.
(let ((guix-env (getenv "GUIX_ENVIRONMENT")))
  ;; XXX `guix-package-enable-at-startup' should be removed from
  ;; "guix-emacs.el" soon.
  (setq guix-package-enable-at-startup nil)
  (when (and guix-env
             (require 'guix-emacs nil t))
    (guix-emacs-autoload-packages guix-env)))
