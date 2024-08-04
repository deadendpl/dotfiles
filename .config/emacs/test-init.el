(setq user-emacs-directory "~/.config/emacs/test")

(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)
  (use-package-always-defer t))

(use-package package
  :custom
  (package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("elpa" . "https://elpa.gnu.org/packages/")
                      ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
                      ;; ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                      ))
  (package-user-dir (expand-file-name "packages/" user-emacs-directory))
  (package-gnupghome-dir (expand-file-name "gpg" user-emacs-directory))
  (package-async t)
  :init
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  )

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(load-file (expand-file-name "init.el" user-emacs-directory))
