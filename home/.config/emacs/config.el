(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" "~/.local/share/emacs/"))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (call-process "git" nil buffer t "clone"
				       (plist-get order :repo) repo)))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor)

;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
;;(elpaca nil (message "deferred"))

(use-package general
  :config
  (general-evil-setup)

;; set up 'SPC' as the global leader key
(general-create-definer custom/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC" ;; set leader
  :global-prefix "M-SPC") ;; access leader in insert mode

(custom/leader-keys
  "SPC" '(projectile-find-file :wk "Find file in project")
  "." '(find-file :wk "Find file")
  "TAB TAB" '(comment-line :wk "Comment lines"))

(custom/leader-keys
  "f" '(:ignore t :wk "Files")
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
  "f r" '(counsel-recentf :wk "Find recent files")
  "f u" '(sudo-edit-find-file :wk "Sudo find file")
  "f U" '(sudo-edit :wk "Sudo edit file"))

(custom/leader-keys
  "b" '(:ignore t :wk "Buffer")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b k" '(kill-this-buffer :wk "Kill this buffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer")
  "b i" '(ibuffer :wk "List all buffers"))

(custom/leader-keys
  "d" '(:ignore t :wk "Dired")
  "d d" '(dired :wk "Open dired")
  "d j" '(dired-jump :wk "Dired jump to current")
  "d n" '(neotree-dir :wk "Open directory in neotree")
  "d p" '(peep-dired :wk "Peep-dired"))

(custom/leader-keys
  "RET" '(bookmark-jump :wk "Go to bookmark")
  "b m" '(bookmark-set :wk "Set bookmark"))

(custom/leader-keys
  "e" '(:ignore t :wk "Eshell/Evaluate")    
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate and elisp expression")
  "e h" '(counsel-esh-history :which-key "Eshell history")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region")
  "e s" '(eshell :which-key "Eshell"))

(custom/leader-keys
  "h" '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h t" '(load-theme :wk "Load theme")
  "h v" '(describe-variable :wk "Describe variable")
  ;;"h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config"))
  "h r r" '(reload-init-file :wk "Reload emacs config"))

(custom/leader-keys
  "m" '(:ignore t :wk "Org")
  "m a" '(org-agenda :wk "Org agenda")
  "m e" '(org-export-dispatch :wk "Org export dispatch")
  "m i" '(org-toggle-item :wk "Org toggle item")
  "m t" '(org-todo :wk "Org todo")
  "m B" '(org-babel-tangle :wk "Org babel tangle")
  "m T" '(org-todo-list :wk "Org todo list"))

(custom/leader-keys
  "m b" '(:ignore t :wk "Tables")
  "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

(custom/leader-keys
  "m d" '(:ignore t :wk "Date/deadline")
  "m d t" '(org-time-stamp :wk "Org time stamp"))

(custom/leader-keys
  "p" '(projectile-command-map :wk "Projectile"))

(custom/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t n" '(neotree-toggle :wk "Toggle neotree")
  "t t" '(visual-line-mode :wk "Toggle truncated lines")
  "t i" '(imenu-list-smart-toggle :wk "Toggle imenu list")
  "t v" '(vterm-toggle :wk "Toggle vterm"))

(custom/leader-keys
  "w" '(:ignore t :wk "Windows")
  ;; Window splits
  "w c" '(evil-window-delete :wk "Close window")
  "w n" '(evil-window-new :wk "New window")
  "w s" '(evil-window-split :wk "Horizontal split window")
  "w v" '(evil-window-vsplit :wk "Vertical split window")
  ;; Window motions
  "w h" '(evil-window-left :wk "Window left")
  "w j" '(evil-window-down :wk "Window down")
  "w k" '(evil-window-up :wk "Window up")
  "w l" '(evil-window-right :wk "Window right")
  "w w" '(evil-window-next :wk "Go to next window")
  ;; Move Windows
  "w H" '(buf-move-left :wk "Buffer move left")
  "w J" '(buf-move-down :wk "Buffer move down")
  "w K" '(buf-move-up :wk "Buffer move up")
  "w L" '(buf-move-right :wk "Buffer move right"))
)

(defun emacs-counsel-launcher ()
  "Create and select a frame called emacs-counsel-launcher which consists only of a minibuffer and has specific dimensions. Runs counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame 
    (make-frame '((name . "emacs-run-launcher")
                  (minibuffer . only)
                  (fullscreen . 0) ; no fullscreen
                  (undecorated . t) ; remove title bar
                  ;;(auto-raise . t) ; focus on this frame
                  ;;(tool-bar-lines . 0)
                  ;;(menu-bar-lines . 0)
                  (internal-border-width . 10)
                  (width . 80)
                  (height . 11)))
                  (unwind-protect
                    (counsel-linux-app)
                    (delete-frame))))

(use-package app-launcher
  :elpaca '(app-launcher :host github :repo "SebastienWae/app-launcher"))
;; create a global keyboard shortcut with the following code
;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"

(defun emacs-run-launcher ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame 
    (make-frame '((name . "emacs-run-launcher")
                  (minibuffer . only)
                  (fullscreen . 0) ; no fullscreen
                  (undecorated . t) ; remove title bar
                  ;;(auto-raise . t) ; focus on this frame
                  ;;(tool-bar-lines . 0)
                  ;;(menu-bar-lines . 0)
                  (internal-border-width . 10)
                  (width . 80)
                  (height . 11)))
                  (unwind-protect
                    (app-launcher-run-app)
                    (delete-frame))))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(require 'windmove)

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.config/doom/ricky.jpg")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  ;;(dashboard-modify-heading-icons '((recents . "file-text")
  ;;                            (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package diminish)

(setq dired-listing-switches "-la --group-directories-first")

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "feh")
                                ("jpg" . "feh")
                                ("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

;;(use-package dired
;;  :elpaca nil
;;  :config
;;    (setq dired-listing-switches "-la --group-directories-first")
;;    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
;;    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)) ; use dired-find-file instead if not using dired-open package

;;(use-package evil
;;  :elpaca nil    
;;  :config
;;    (evil-define-key 'normal dired-mode-map
;;      "h" 'dired-up-directory
;;      "l" 'dired-open-file))

;;(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package emojify
  :init (global-emojify-mode 1)
  :custom
    (emojify-emojis-dir "~/.local/share/emacs/elpaca/builds/emojify/emojis"))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(set-face-attribute 'default nil
  :font "CodeNewRoman Nerd Font Mono"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu Nerd Font"
  :height 90
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "CodeNewRoman Nerd Font Mono"
  :height 90
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "CodeNewRoman Nerd Font Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(use-package imenu-list
 :custom
   (imenu-list-focus-after-activation t
    imenu-list-auto-resize t))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package lua-mode)
(use-package nix-mode)

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 35
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

;; show hidden files

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-superstar
  :init (add-hook 'org-mode-hook 'org-superstar-mode t))

(use-package org-auto-tangle
  :defer t
  :diminish
  :hook (org-mode . org-auto-tangle-mode))

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(require 'org-tempo)

(use-package evil-org
  :ensure t
  :diminish
  :after org
  :init
    (add-hook 'org-mode-hook 'evil-org-mode t))
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
;;  :hook (org-mode . (lambda () evil-org-mode))
;;  :config
;;  (require 'evil-org-agenda)
;;  (evil-org-agenda-set-keys)

;;(after! evil-org
;;  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(setq org-insert-heading-respect-content nil)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(use-package company-org-block
  :after org)

(use-package projectile
  :config
  (projectile-mode 1))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(defun reload-init-file ()
  "Loading the user-init-file twice, which is a hack because for some reason, just loading the user-init-file once does not work properly."
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
;; eshell-aliases-file -- sets an aliases file for the eshell.
  
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000))

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

(use-package sudo-edit)

(use-package dracula-theme)
;;  :ensure "~/.local/share/emacs/themes/dracula-theme.el")

;;(add-to-list 'custom-theme-load-path "~/.local/share/emacs/themes/")
;;(load-theme 'dracula t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit nil
	which-key-separator " → " ))
