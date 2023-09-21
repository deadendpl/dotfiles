;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "CodeNewRoman Nerd Font" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "GoMono Nerd Font" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'ewal-doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . (lambda () (all-the-icons-ibuffer-mode t))))

(use-package imenu-list
  :defer t
  :config
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t))

(use-package beacon
  :custom
    (beacon-mode 1))

(use-package rainbow-mode
  :hook org-mode prog-mode)

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package org-superstar
  :defer t
  :init (add-hook 'org-mode-hook 'org-superstar-mode t))

(use-package org-auto-tangle
  :defer t
  :diminish
  :hook (org-mode . org-auto-tangle-mode))

(use-package company-org-block
  :defer t
  :after org)

(use-package org
  :defer t
  :init
    (setq org-directory "~/org/"
          org-agenda-files '("agenda.org"))
  :custom-face
    ;; setting size of headers
    (org-document-title ((t (:inherit outline-1 :height 1.7))))
    (org-level-1 ((t (:inherit outline-1 :height 1.7))))
    (org-level-2 ((t (:inherit outline-2 :height 1.6))))
    (org-level-3 ((t (:inherit outline-3 :height 1.5))))
    (org-level-4 ((t (:inherit outline-4 :height 1.4))))
    (org-level-5 ((t (:inherit outline-5 :height 1.3))))
    (org-level-6 ((t (:inherit outline-5 :height 1.2))))
    (org-level-7 ((t (:inherit outline-5 :height 1.1))))
  :custom
    (org-insert-heading-respect-content nil)
    (org-hide-emphasis-markers t)
    (org-hide-leading-stars t)
    (org-hide-emphasis-markers t)
    (org-startup-with-inline-images t)
    (org-ellipsis " •")
    (org-agenda-window-setup 'current-window)
    (org-agenda-block-separator 8411))

;;(setq fancy-splash-image "~/.config/doom/ricky.jpg")

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '"PP Poo Poo") ;;the important line
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(defun load-doom-config ()
  "Loads doom configuration file which is ~/.config/doom/config.org."
  (interactive)
  (find-file "~/.config/doom/config.org"))

(map! :leader
  (:prefix ("t" . "toggle")
    :desc "Imenu list" "i" #'imenu-list-smart-toggle
    :desc "Vterm" "v" #'vterm-toggle)
  (:prefix ("f" . "file")
    :desc "Open config.org" "P" #'load-doom-config)
  (:prefix ("d" . "dired")
    :desc "Open dired" "d" #'dired
    :desc "Dired jump to current" "j" #'dired-jump
    :desc "Open directory in neotree" "n" #'neotree-dir
    :desc "Peep-dired" "n" #'peep-dired)
  (:prefix ("e" . "eshell/evaluate")
    :desc "Evaluate elisp in buffer" "b" #'eval-buffer
    :desc "Evaluate defun containing or after point" "d" #'eval-defun
    :desc "Evaluate and elisp expression" "e" #'eval-expression
    :desc "Eshell history" "h" #'counsel-esh-history
    :desc "Evaluate elisp expression before point" "l" #'eval-last-sexp
    :desc "Evaluate elisp in region" "r" #'eval-region
    :desc "Eshell" "s" #'eshell))
