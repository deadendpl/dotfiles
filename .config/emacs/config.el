(scroll-bar-mode -1)                 ; Disable visible scrollbar
(tool-bar-mode -1)                   ; Disable the toolbar
(tooltip-mode -1)                    ; Disable tooltips
(menu-bar-mode -1)                   ; Disable the menu bar
(set-fringe-mode 10)                 ; Give some breathing room
(global-auto-revert-mode t)          ; Automatically show changes if the file has changed
(global-display-line-numbers-mode 1) ; Display line numbers
(global-visual-line-mode t)          ; Enable truncated lines
(delete-selection-mode 1)            ; You can select text and delete it by typing.
(electric-pair-mode 1)               ; Turns on automatic parens pairing
(electric-indent-mode -1)            ; Turn off the weird indenting that Emacs does by default.

;; This folder is for everything that clutters user-emacs-directory
(defvar user-share-emacs-directory "~/.local/share/emacs/"
  "A lot of stuff normally clutters user-emacs-directory.
In order to prevent that this variable exists.
Most of the stuff will get redirected here.")

(setq visible-bell nil ;; Set up the visible bell
      inhibit-startup-message nil ; default emacs startup message
      custom-file (concat user-share-emacs-directory "custom.el") ; custom settings that emacs autosets put into it's own file
      backup-directory-alist '((".*" . "~/.local/share/Trash/files")) ; moving backup files to trash directory
      recentf-save-file (concat user-share-emacs-directory "recentf") ; recentf file put somewhere else
      bookmark-default-file (concat user-share-emacs-directory "bookmarks") ; bookmarks file put somewhere else
      auto-save-list-file-name (concat user-share-emacs-directory "auto-save-list/list")
      global-auto-revert-non-file-buffers t ; refreshing buffers when files have changed
      use-dialog-box nil        ; turns off graphical dialog boxes
      tramp-persistency-file-name (concat user-share-emacs-directory "tramp") ; tramp file put somewhere else
      initial-major-mode 'fundamental-mode ; setting scratch buffer in fundamental mode
      initial-scratch-message nil)         ; deleting scratch buffer message

;; Make ESC quit prompts immediately
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; turn off line numbers in certain modes
(dolist (mode '(neotree-mode-hook
		vterm-mode-hook
                term-mode-hook
                shell-mode-hook
		helpful-mode-hook
		dashboard-mode-hook
		dired-mode-hook
                org-agenda-mode-hook
                which-key-mode-hook
                tldr-mode-hook
                dictionary-mode-hook
                Man-mode-hook
                woman-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; locking buffers from killing
(with-current-buffer "*scratch*" 
	  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
	  (emacs-lock-mode 'kill))

;; Initialize package sources
(require 'package)

(setq package-user-dir (concat user-share-emacs-directory "packages/")
      package-gnupghome-dir (concat user-share-emacs-directory "gpg")
      package-async t
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(use-package command-log-mode
  :disabled)

;;(defun custom/evil-hook ()
;;  (dolist (mode '(custom-mode
;;                  eshell-mode
;;                  git-rebase-mode
;;                  erc-mode
;;                  circe-server-mode
;;                  circe-chat-mode
;;                  circe-query-mode
;;                  sauron-mode
;;                  term-mode))
;;   (add-to-list 'evil-emacs-state-modes mode)))


(use-package evil
  :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
	  evil-want-C-u-scroll t
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode)
  :config
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
    ;; (define-key evil-motion-state-map (kbd "/") 'swiper))
    ;; (dolist (mode '(eshell-mode
    ;;                 term-mode
    ;;                 vterm-mode))
    ;; (add-to-list 'evil-emacs-state-modes mode)))


(use-package evil-collection
  :after evil
  :config
    ;; Do not uncomment this unless you want to specify each and every mode
    ;; that evil-collection should works with.  The following line is here 
    ;; for documentation purposes in case you need it.  
    ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
    (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
    (evil-collection-init))

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
  "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
  "TAB TAB" '(comment-line :wk "Comment lines")
  "u" '(universal-argument :wk "Universal argument"))

(custom/leader-keys
  "b" '(:ignore t :wk "Bookmarks/Buffers")
  "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
  "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
  "b d" '(bookmark-delete :wk "Delete bookmark")
  "b i" '(ibuffer :wk "Ibuffer")
  "b I" '(counsel-switch-buffer-other-window :wk "Switch buffer")
  "b k" '(kill-this-buffer :wk "Kill this buffer")
  "b K" '(kill-some-buffers :wk "Kill multiple buffers")
  "b l" '(list-bookmarks :wk "List bookmarks")
  "b m" '(bookmark-set :wk "Set bookmark")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer")
  "b R" '(rename-buffer :wk "Rename buffer")
  "b s" '(basic-save-buffer :wk "Save buffer")
  "b S" '(save-some-buffers :wk "Save multiple buffers")
  "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

(custom/leader-keys
  "RET" '(bookmark-jump :wk "Go to bookmark"))

(custom/leader-keys
  "d" '(:ignore t :wk "Dired")
  "d d" '(dired :wk "Open dired")
  "d j" '(dired-jump :wk "Dired jump to current")
  "d n" '(neotree-dir :wk "Open directory in neotree")
  "d p" '(peep-dired :wk "Peep-dired"))

(custom/leader-keys
  "e" '(:ignore t :wk "Eshell/Evaluate")
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate and elisp expression")
  "e h" '(counsel-esh-history :wk "Eshell history")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region")
  "e s" '(eshell :wk "Eshell"))

(custom/leader-keys
  "f" '(:ignore t :wk "Files")
  "f c" '((lambda () (interactive)
            (find-file "~/.config/emacs/config.org"))
          :wk "Open emacs config.org")
  "f e" '((lambda () (interactive)
            (dired "~/.config/emacs/"))
          :wk "Open user-emacs-directory in dired")
  "f d" '(find-grep-dired :wk "Search for string in files in DIR")
  "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
  "f i" '((lambda () (interactive)
            (find-file "~/.config/emacs/init.el"))
          :wk "Open emacs init.el")
  "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
  "f l" '(counsel-locate :wk "Locate a file")
  "f p" '(counsel-find-file (user-emacs-directory) :wk "Config directory")
  "f r" '(counsel-recentf :wk "Find recent files")
  "f u" '(sudo-edit-find-file :wk "Sudo find file")
  "f U" '(sudo-edit :wk "Sudo edit file"))

(custom/leader-keys
  "g" '(:ignore t :wk "Git")
  "g /" '(magit-displatch :wk "Magit dispatch")
  "g ." '(magit-file-displatch :wk "Magit file dispatch")
  "g b" '(magit-branch-checkout :wk "Switch branch")
  "g c" '(:ignore t :wk "Create")
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
  "g C" '(magit-clone :wk "Clone repo")
  "g f" '(:ignore t :wk "Find")
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
  "g F" '(magit-fetch :wk "Git fetch")
  "g g" '(magit-status :wk "Magit status")
  "g i" '(magit-init :wk "Initialize git repo")
  "g l" '(magit-log-buffer-file :wk "Magit buffer log")
  "g r" '(vc-revert :wk "Git revert file")
  "g s" '(magit-stage-file :wk "Git stage file")
  "g t" '(git-timemachine :wk "Git time machine")
  "g u" '(magit-stage-file :wk "Git unstage file"))

(custom/leader-keys
  "G" '(:ignore :wk "Games")
  "G m" '(minesweeper :wk "Minesweeper"))

(custom/leader-keys
  "h" '(:ignore t :wk "Help")
  "h a" '(counsel-apropos :wk "Apropos")
  "h b" '(describe-bindings :wk "Describe bindings")
  "h c" '(describe-char :wk "Describe character under cursor")
  "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
  "h e" '(view-echo-area-messages :wk "View echo area messages")
  "h f" '(describe-function :wk "Describe function")
  "h F" '(describe-face :wk "Describe face")
  "h g" '(describe-gnu-project :wk "Describe GNU Project")
  "h i" '(info :wk "Info")
  "h I" '(describe-input-method :wk "Describe input method")
  "h k" '(describe-key :wk "Describe key")
  "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
  "h L" '(describe-language-environment :wk "Describe language environment")
  "h m" '(describe-mode :wk "Describe mode")
  "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config")
    "h r t" '((lambda () (interactive) (load-theme real-theme t)) :wk "Reload theme")
  "h t" '(load-theme :wk "Load theme")
  "h v" '(describe-variable :wk "Describe variable")
  "h w" '(where-is :wk "Prints keybinding for command if set")
  "h x" '(describe-command :wk "Display full documentation for command"))
    
(custom/leader-keys
  "m" '(:ignore t :wk "Org")
  "m a" '(org-agenda :wk "Org agenda")
  "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table")
    "m b a" '(org-table-align :wk "Align table")
    "m b b" '(org-table-blank-field :wk "Make blank field")
    "m b c" '(org-table-create-or-convert-from-region :wk "Create/Convert from region")
    "m b e" '(org-table-edit-field :wk "Edit field")
    "m b f" '(org-table-edit-formulas :wk "Edit fromulas")
    "m b h" '(org-table-field-info :wk "Field info")
    "m b s" '(org-table-sort-lines :wk "Sort lines")
    "m b r" '(org-table-recalculate :wk "Recalculate")
    "m b R" '(org-table-recalculate-buffer-tables :wk "Recalculate buffer tables")
    "m b d" '(:ignore t :wk "delete")
      "m b d c" '(org-table-delete-column :wk "Delete column")
      "m b d r" '(org-table-kill-row :wk "Delete row")
    "m b i" '(:ignore t :wk "insert")
      ;; "m b i c" ('org-table-insert-column :wk "Insert column") FIXME
      ;; "m b i h" ('org-table-insert-hline :wk "Insert horizontal line") FIXME
      ;; "m b i r" ('org-table-insert-row :wk "Insert row") FIXME
      ;; "m b i H" ('org-table-hline-and-move :wk "Insert horizontal line and move") FIXME
  "m d" '(:ignore t :wk "Date/deadline")
    "m d d" '(org-deadline :wk "Org deadline")
    "m d s" '(org-schedule :wk "Org schedule")
    "m d t" '(org-time-stamp :wk "Org time stamp")
    "m d T" '(org-time-stamp-inactive :wk "Org time stamp inactive")
  "m e" '(org-export-dispatch :wk "Org export dispatch")
  "m i" '(org-toggle-item :wk "Org toggle item")
  "m l" '(:ignore t :wk "Link")
    "m l l" '(org-insert-link :wk "Insert link")
  "m t" '(org-todo :wk "Org todo")
  "m B" '(org-babel-tangle :wk "Org babel tangle")
  "m T" '(org-todo-list :wk "Org todo list"))


(custom/leader-keys
  "n" '(:ignore t :wk "Notes")
  "n d" '(org-notes-dired :wk "Open notes folder"))

(custom/leader-keys
  "o" '(:ignore t :wk "Open")
  "o d" '(dashboard-open :wk "Dashboard")
  "o e" '(elfeed :wk "Elfeed RSS")
  "o f" '(make-frame :wk "Open buffer in new frame")
  "o F" '(select-frame-by-name :wk "Select frame by name"))

(custom/leader-keys
  "p" '(projectile-command-map :wk "Projectile"))

(custom/leader-keys
  "s" '(:ignore t :wk "Search")
  "s d" '(dictionary-search :wk "Search dictionary")
  "s m" '(man :wk "Man pages")
  "s t" '(tldr :wk "Lookup TLDR docs for a command")
  "s w" '(woman :wk "Similar to man but doesn't require man"))

(custom/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t e" '(eshell-toggle :wk "Toggle eshell")
  "t f" '(flycheck-mode :wk "Toggle flycheck")
  "t i" '(imenu-list-smart-toggle :wk "Toggle imenu list")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t n" '(neotree-toggle :wk "Toggle neotree")
  "t r" '(rainbow-mode :wk "Toggle rainbow mode")
  "t t" '(visual-line-mode :wk "Toggle truncated lines")
  "t v" '(vterm-toggle :wk "Toggle vterm")
  "t z" '(zen-mode :wk "Toggle zen mode"))

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

;; text resizing
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :defer t
  :after dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package all-the-icons-ibuffer
  :defer t
  :after ibuffer
  :hook (ibuffer-mode . (lambda () (all-the-icons-ibuffer-mode t))))

(use-package nerd-icons)

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1))

(use-package beacon
  :custom
    (beacon-mode 1))

(use-package buffer-move
  :defer t)

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
  :custom
    (initial-buffer-choice (lambda () (dashboard-open)))
    (dashboard-startup-banner "~/.config/emacs/banner.txt")
    (dashboard-center-content t)
    (dashboard-items '((recents  . 5)
                       (bookmarks . 5)
                       (projects . 5)
                       (agenda . 5)))
                       ;; (registers . 5)
  :config
    (dashboard-setup-startup-hook)
  :bind
    (:map dashboard-mode-map
      ([remap dashboard-next-line] . 'widget-forward)
      ([remap dashboard-previous-line] . 'widget-backward)
      ("up" . 'widget-forward)
      ("down" . 'widget-backward)))

(use-package dired
  :ensure nil
  :defer t
  :custom
    (insert-directory-program "ls")
    (dired-listing-switches "-la --group-directories-first")
  :config
    (evil-collection-dired-setup)
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file))

(use-package dired-open
  :after dired
  :defer t
  :config
    (setq dired-open-extensions '(("gif" . "swaiymg")
                                  ("jpg" . "swaiymg")
                                  ("png" . "swaiymg")
                                  ("mkv" . "mpv")
                                  ("mp4" . "mpv"))))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package emojify
  :init (global-emojify-mode 1)
  :custom
    (emojify-emojis-dir (concat user-share-emacs-directory "emojis")))

(set-face-attribute 'default nil
  :font "CodeNewRoman Nerd Font Mono"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu Nerd Font"
  :height 100
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
;; (set-face-attribute 'font-lock-keyword-face nil
;;   :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "CodeNewRoman Nerd Font Mono-9"))

;; Uncomment the following line if line spacing needs adjusting.
;; (setq-default line-spacing 0.12)

(use-package flycheck
  :after prog-mode
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package minesweeper
  :defer t
  :config
    (evil-set-initial-state 'minesweeper-mode 'emacs))

(use-package magit
  :defer t)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package helpful
  :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
  :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

(use-package imenu-list
  :defer t
  :custom
    (imenu-list-focus-after-activation t
     imenu-list-auto-resize t))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
    (("C-c C-r" . ivy-resume)
     ("C-x B" . ivy-switch-buffer-other-window)
     ("C-s" . swiper)
    :map ivy-minibuffer-map
      ("TAB" . ivy-alt-done)
      ("C-l" . ivy-alt-done)
      ("C-j" . ivy-next-line)
      ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
      ("C-k" . ivy-previous-line)
      ("C-l" . ivy-done)
      ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
      ("C-k" . ivy-previous-line)
      ("C-d" . ivy-reverse-i-search-kill))
  :diminish
  :custom
    (ivy-use-virtual-buffers t
     ivy-count-format "(%d/%d) "
     enable-recursive-minibuffers t)
  :config
    (ivy-mode))
    
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

(use-package counsel
  :bind
    (("M-x" . counsel-M-x)
     ;; ([remap ibuffer] . counsel-ibuffer)
     ("C-x C-f" . counsel-find-file)
    :map minibuffer-local-map
      ("C-r" . 'counsel-minibuffer-history)))


(use-package counsel
  :after ivy
  :diminish
  :bind
    (("M-x" . counsel-M-x)
     ("C-x b" . counsel-ibuffer)
     ("C-x C-f" . counsel-find-file)
      :map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history))
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package hl-todo
  :defer t
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :custom
    (hl-todo-highlight-punctuation ":")
    (hl-todo-keyword-faces
    `(("TODO"       warning bold)
      ("FIXME"      error bold)
      ("HACK"       font-lock-constant-face bold)
      ("REVIEW"     font-lock-keyword-face bold)
      ("NOTE"       success bold)
      ("DEPRECATED" font-lock-doc-face bold))))

(use-package lua-mode
  :defer t)
(use-package nix-mode
  :defer t)
(use-package markdown-mode
  :defer t)

(use-package neotree
  :defer t
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

(use-package evil-org
  :diminish
  :defer t
  :after org
  :init
    (setq org-return-follows-link t)
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
  :config
    ;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map (kbd "SPC") nil)
      (define-key evil-motion-state-map (kbd "RET") 'org-return)
      (define-key evil-motion-state-map (kbd "TAB") nil)))
    ;; Setting RETURN key in org-mode to follow links
    ;; (setq org-return-follows-link t)
    ;; (require 'evil-org-agenda)
    ;; (evil-org-agenda-set-keys))

;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(require 'org-tempo)

(use-package org-superstar
  :defer t
  :after org
  :init (add-hook 'org-mode-hook 'org-superstar-mode t))

(use-package org-auto-tangle
  :defer t
  :after org
  :diminish
  :hook (org-mode . org-auto-tangle-mode))

(setq org-edit-src-content-indentation 0)

(use-package toc-org
  :defer t
  :after org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(defun org-notes-dired ()
  "Opens org-directory in Dired."
  (interactive)
  (dired (concat org-directory "/")))

(use-package org
  :defer t
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
    (org-directory "~/org/")
    (org-agenda-files '("agenda.org"))
    (org-todo-keywords
     '((sequence
        "TODO(t)"  ; A task that needs doing & is ready to do
        "PROJ(p)"  ; A project, which usually contains other tasks
        "LOOP(r)"  ; A recurring task
        "STRT(s)"  ; A task that is in progress
        "WAIT(w)"  ; Something external is holding up this task
        "HOLD(h)"  ; This task is paused/on hold because of me
        "IDEA(i)"  ; An unconfirmed and unapproved task or notion
        "|"
        "DONE(d)"  ; Task successfully completed
        "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
       (sequence
        "[ ](T)"   ; A task that needs doing
        "[-](S)"   ; Task is in progress
        "[?](W)"   ; Task is being held up or paused
        "|"
        "[X](D)")  ; Task was completed
       (sequence
        "|"
        "OKAY(o)"
        "YES(y)"
        "NO(n)")))
      (org-insert-heading-respect-content nil)
      (org-hide-emphasis-markers t)
      (org-hide-leading-stars t)
      (org-hide-emphasis-markers t)
      (org-startup-with-inline-images t)
      (org-ellipsis " •")
      (org-agenda-window-setup 'current-window)
      (org-agenda-block-separator 8411))
  :config

(use-package company-org-block
  :defer t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package perspective
  :disabled
  :custom
    ;; NOTE! I have also set 'SCP =' to open the perspective menu.
    ;; I'm only setting the additional binding because setting it
    ;; helps suppress an annoying warning message.
    (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
    (persp-mode)
  :config
    ;; Sets a file to write to when we save states
    (setq persp-state-default-file (concat user-share-emacs-directory "sessions")))

    ;; This will group buffers by persp-name in ibuffer.
    (add-hook 'ibuffer-hook
              (lambda ()
                (persp-ibuffer-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))

    ;; Automatically save perspective states to file when Emacs exits.
    (add-hook 'kill-emacs-hook #'persp-state-save)

(use-package projectile
  :diminish projectile-mode
  :custom
    (projectile-known-projects-file (concat user-share-emacs-directory "projectile-bookmarks.eld"))
  :config (projectile-mode)
  :bind-keymap
    ("C-c p" . projectile-command-map)
  :init
    (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
    (counsel-projectile-mode 1))

(use-package rainbow-delimiters
  :defer t
  :after prog-mode)

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package company-shell
  :after sh-mode
  :custom
    (add-to-list 'company-backends 'company-shell)
    (add-to-list 'company-backends 'company-shell-env))

(use-package eshell
  :defer t
  :custom
    (eshell-rc-script "~/.config/eshell/profile")     ;; your profile for eshell; like a bashrc for eshell.
    (eshell-aliases-file "~/.config/eshell/aliases") ;; sets an aliases file for the eshell.
    (eshell-history-file-name (concat user-share-emacs-directory "eshell-history"))
    (eshell-last-dir-ring-file-name (concat user-share-emacs-directory "eshell-lastdir"))
    (eshell-history-size 5000)
    (eshell-buffer-maximum-lines 5000)
    (eshell-hist-ignoredups t)
    (eshell-scroll-to-bottom-on-input t)
    (eshell-destroy-buffer-when-process-dies t)
    (eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))
  :config
    (evil-set-initial-state 'eshell-mode 'emacs))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
    (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-vterm
  :after eshell
  :config
    (eshell-vterm-mode))

(use-package vterm
  :defer t
  :config
    (setq shell-file-name "/bin/sh"
          vterm-max-scrollback 5000))
    (add-hook 'vterm-mode-hook (lambda () (setq evil-default-state 'emacs)))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p t)
  (vterm-toggle-scope 'project)
  :config
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

(use-package sudo-edit
  :defer t)

(use-package ewal)
(use-package ewal-doom-themes)
(use-package doom-themes
  :ensure t
  :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    
    (setq real-theme 'ewal-doom-one) ;; NOTE this is where you should set your theme
    (load-theme real-theme t)
  
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    ;;(doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth

(use-package tldr
  :defer t
  :init
    (setq tldr-directory-path (concat user-share-emacs-directory "tldr/")))

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
	which-key-separator " → "
        which-key-idle-delay 0.5))

(use-package yasnippet
  :defer t
  :after prog-mode)

(defun zen-mode ()
  "Comfy writing experience"
  (interactive)
  (set-fringe-mode 200)
  (display-line-numbers-mode 0))
