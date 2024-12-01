(setq frame-inhibit-implied-resize t ;; that makes startup slightly faster
      auto-mode-case-fold nil
      package-native-compile t
      default-directory "~/"
      use-package-enable-imenu-support t
      )

;; improving startup with setting garbage collection
;; https://codeberg.org/zyd/dotfiles/src/commit/e2deef9551ec259e62e19abe3a9b86feb4a5c870/emacs.d/early-init.el
(let ((default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)
  (add-hook 'after-init-hook
            `(lambda ()
               (setq gc-cons-percentage ,default-gc-percentage
                     gc-cons-threshold ,default-gc-threshold))))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
