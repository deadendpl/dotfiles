(defun custom/termux-p ()
  "Checks if Emacs is running inside of Termux."
  (and (getenv "TERMUX_VERSION")))

(unless (custom/termux-p)
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1)))         ; Disable visible scrollbar
(unless (custom/termux-p)
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1)))           ; Disable the toolbar
(tooltip-mode -1)                    ; Disable tooltips
(menu-bar-mode -1)                   ; Disable the menu bar
(unless (custom/termux-p)
  (if (fboundp 'set-fringe-mode)
        (set-fringe-mode 10)))       ; Give some breathing room
(global-auto-revert-mode t)          ; Automatically show changes if the file has changed
(global-visual-line-mode t)          ; Enable truncated lines (line wrapping)
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; Line numbers in programming modes
(delete-selection-mode 1)            ; You can select text and delete it by typing (in emacs keybindings).
(electric-pair-mode 0)               ; Turns off automatic parens pairing
(electric-indent-mode -1)            ; Turn off the weird indenting that Emacs does by default.
(column-number-mode 1)               ; Column number in modeline
;; (fset 'yes-or-no-p 'y-or-n-p)        ; Simplyfying yes or no prompts
(save-place-mode 1)                  ; Saving last place in file
(display-battery-mode 1)             ; Setting battery percentage in modeline
(indent-tabs-mode 0)                 ; Using spaces instead of tabs for indentation

;; This folder is for everything that clutters user-emacs-directory
(defvar custom/user-share-emacs-directory "~/.local/share/emacs/"
  "Directory to redirect cache/dump files.
Elisp packages cache folders/files normally clutter user-emacs-directory.
The same goes for some default files like bookmarks file.
In order to prevent that this variable exists.
Most of the stuff will get redirected here.")

(setq-default recentf-save-file (expand-file-name "recentf" custom/user-share-emacs-directory) ; recentf file put somewhere else
              bookmark-default-file (expand-file-name "bookmarks" custom/user-share-emacs-directory) ; bookmarks file put somewhere else
              elfeed-db-directory (expand-file-name "elfeed" custom/user-share-emacs-directory) ; elfeed cache? directory
              auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" custom/user-share-emacs-directory)
              prescient-save-file (expand-file-name "var/prescient-save.el" custom/user-share-emacs-directory)
              custom-file (expand-file-name "custom.el" custom/user-share-emacs-directory) ; custom settings that emacs autosets put into it's own file
              backup-directory-alist '((".*" . "~/.local/share/Trash/files")) ; moving backup files to trash directory
              tramp-persistency-file-name (expand-file-name "tramp" custom/user-share-emacs-directory) ; tramp file put somewhere else
              save-place-file (expand-file-name "places" custom/user-share-emacs-directory)
              url-configuration-directory (expand-file-name "url" custom/user-share-emacs-directory) ; cache from urls (eww)
              multisession-directory (expand-file-name "multisession" custom/user-share-emacs-directory)
              transient-history-file (expand-file-name "transient/history.el" custom/user-share-emacs-directory))

(setq visible-bell nil ;; Set up the visible bell
      inhibit-startup-message nil ; default emacs startup message
      recentf-max-saved-items nil ; infinite amount of entries in recentf file
      recentf-auto-cleanup 'never ; not cleaning recentf file
      global-auto-revert-non-file-buffers t ; refreshing buffers when files have changed
      use-dialog-box nil ; turns off graphical dialog boxes
      initial-major-mode 'fundamental-mode ; setting scratch buffer in fundamental mode
      initial-scratch-message nil ; deleting scratch buffer message
      scroll-conservatively 1000 ; Scroll one line at a time
      scroll-margin 1 ; Keep a margin of 1 line when scrolling at the window's edge
      tab-always-indent nil
      vc-follow-symlinks t ; Enable follow symlinks
      indent-tabs-mode nil ; use spaces instead of tabs for indenting
      standard-indent 2 ; indenting set to 2
      auto-revert-interval 1
      display-line-numbers-type 'relative
      use-short-answers t ; replace yes-no prompts with y-n
      fast-but-imprecise-scrolling t ; fast scrolling
      inhibit-compacting-font-caches t
      sentence-end-double-space nil ; sentences end with 1 space
      create-lockfiles nil ; no files wiht ".#"
      switch-to-buffer-obey-display-actions t) ; swtich-to-buffer will respect display-buffer-alist
      ;; auto-save-list-file-name (concat custom/user-share-emacs-directory "auto-save-list/list")
(if (custom/termux-p)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; (defun quit-window (&optional kill window)
;;   "Quit WINDOW, deleting it, and kill its buffer.
;; WINDOW must be a live window and defaults to the selected one.
;; The buffer is killed instead of being buried.
;; This function ignores the information stored in WINDOW's `quit-restore' window parameter."
;;   (interactive "P")
;;   (set-window-parameter window 'quit-restore `(frame frame nil ,(current-buffer)))
;;   (quit-restore-window window 'kill))

;; Some file extensions set for certain modes
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . conf-colon-mode))

;; locking buffers from killing
(with-current-buffer "*scratch*"
          (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
          (emacs-lock-mode 'kill))

;; Make ESC quit prompts immediately
(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; make utf-8 the coding system
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; Initialize package sources
(require 'package)

(setq package-user-dir (expand-file-name "packages/" custom/user-share-emacs-directory)
      package-gnupghome-dir (expand-file-name "gpg" custom/user-share-emacs-directory)
      package-async t
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t
      use-package-always-ensure t ; packages by default will be lazy loaded, like they will have defer: t
      use-package-always-defer t) ; packages by default will be lazy loaded, like they will have defer: t

(use-package gcmh
  :demand
  :diminish
  :custom
    (gcmh-mode 1)
    (gcmh-idle-delay 10)
    (gcmh-high-cons-threshold (* 32 1024 1024))
    (gc-cons-percentage 0.8))

(use-package quelpa
  :demand
  :custom
    (quelpa-dir (expand-file-name "quelpa/" custom/user-share-emacs-directory))
    (quelpa-checkout-melpa-p nil))
    ;; (use-package-ensure-function 'quelpa))
    ;; (quelpa-build-dir (concat quelpa-dir "build/"))
    ;; (quelpa-melpa-dir (concat quelpa-dir "melpa/"))
    ;; (quelpa-packages-dir (concat quelpa-dir "packages/")))
(use-package quelpa-use-package
  :demand
  :after quelpa)

(use-package evil
  :demand
  :init
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil)
  :custom
    (evil-want-C-u-scroll t)
    (evil-vsplit-window-right t)
    (evil-split-window-below t)
    (evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  ;; :bind
  ;;   (:map evil-normal-state-map
  ;;     ([remap evil-search-forward] . 'swiper))
  :config
    (evil-mode)
    (if (custom/termux-p)
        (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)) ;; for quick save on termux
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
    (evil-define-key 'normal ibuffer-mode-map (kbd "l") 'ibuffer-visit-buffer))
    ;; (define-key evil-motion-state-map (kbd "/") 'swiper))

(use-package evil-collection
  :demand
  :after evil
  :config
    ;; Do not uncomment this unless you want to specify each and every mode
    ;; that evil-collection should works with.  The following line is here
    ;; for documentation purposes in case you need it.
    ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
    (add-to-list 'evil-collection-mode-list 'helpful) ;; evilify help mode
    (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil)

(use-package evil-surround
  :demand
  :after evil
  :config (global-evil-surround-mode 1))

(use-package general
  :config
  (general-evil-setup)

;; set up 'SPC' as the global leader key
(general-create-definer custom/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC" ;; set leader
  :global-prefix "M-SPC") ;; access leader in insert mode

;; for easily quitting in termux
(if (custom/termux-p)
  (custom/leader-keys
    "q" '(evil-quit :wk "Quit Emacs")))

(custom/leader-keys
  "SPC" '(projectile-find-file :wk "Find file in project")
  "." '(find-file :wk "Find file")
  "u" '(universal-argument :wk "Universal argument")
  "x" '(execute-extended-command :wk "M-x"))

(custom/leader-keys
  "TAB" '(:ignore t :wk "Spacing/Indent")
  "TAB TAB" '(evilnc-comment-or-uncomment-lines :wk "Un/Comment lines")
  "TAB SPC" '(untabify :wk "Untabify")
  "TAB DEL" '(whitespace-cleanup :wk "Clean whitespace"))

(custom/leader-keys
  "RET" '(bookmark-jump :wk "Go to bookmark"))

(custom/leader-keys
  "=" '(:ignore t :wk "Tabs/Workspaces")
  "= TAB" '(tab-next :wk "Next tab")
  "= =" '(tab-bar-mode :wk "Enable/Disable")
  "= 1" '((lambda () (interactive) (tab-select 1)) :wk "Tab 1")
  "= 2" '((lambda () (interactive) (tab-select 2)) :wk "Tab 2")
  "= 3" '((lambda () (interactive) (tab-select 3)) :wk "Tab 3")
  "= 4" '((lambda () (interactive) (tab-select 4)) :wk "Tab 4")
  "= 5" '((lambda () (interactive) (tab-select 5)) :wk "Tab 5")
  "= 6" '((lambda () (interactive) (tab-select 6)) :wk "Tab 6")
  "= 7" '((lambda () (interactive) (tab-select 7)) :wk "Tab 7")
  "= 8" '((lambda () (interactive) (tab-select 8)) :wk "Tab 8")
  "= 9" '((lambda () (interactive) (tab-select 9)) :wk "Tab 9")
  "= 0" '((lambda () (interactive) (tab-select 0)) :wk "Tab 0")
  "= t" '(tab-bar-new-tab :wk "New")
  "= d" '(tab-bar-close-tab :wk "Close")
  "= r" '(tab-rename :wk "Rename"))

(custom/leader-keys
  "a" '(:ignore t :wk "Amusement")
  "a b" '(animate-birthday-present :wk "Birthday")
  "a d" '(dissociated-press :wk "Dissoctation")
  "a g" '(:ignore t :wk "Games")
  "a g b" '(bubbles :wk "Bubbles")
  "a g m" '(minesweeper :wk "Minesweeper")
  "a g p" '(pong :wk "Pong")
  "a g s" '(snake :wk "Snake")
  "a g t" '(tetris :wk "Tetris")
  "a e" '(:ignore t :wk "Emoji")
  "a e +" '(emoji-zoom-increase :wk "Zoom in")
  "a e -" '(emoji-zoom-decrease :wk "Zoom out")
  "a e 0" '(emoji-zoom-reset :wk "Zoom reset")
  "a e d" '(emoji-describe :wk "Describe")
  "a e e" '(emoji-insert :wk "Insert")
  "a e i" '(emoji-insert :wk "Insert")
  "a e l" '(emoji-list :wk "List")
  "a e r" '(emoji-recent :wk "Recent")
  "a e s" '(emoji-search :wk "Search")
  "a z" '(zone :wk "Zone"))

(custom/leader-keys
  "b" '(:ignore t :wk "Bookmarks/Buffers")
  "b b" '(switch-to-buffer :wk "Switch to buffer")
  "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
  "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
  "b d" '(bookmark-delete :wk "Delete bookmark")
  "b f" '(scratch-buffer :wk "Scratch buffer")
  "b i" '(ibuffer :wk "Ibuffer")
  "b k" '(kill-current-buffer :wk "Kill current buffer")
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
  "c" '(:ignore t :wk "Compiling")
  "c c" '(compile :wk "Compile")
  "c r" '(recompile :wk "Recompile"))

(custom/leader-keys
  "d" '(:ignore t :wk "Dired")
  "d d" '(dired :wk "Open dired")
  "d h" '(custom/dired-go-to-home :wk "Open home directory")
  "d j" '(dired-jump :wk "Dired jump to current")
  "d n" '(neotree-dir :wk "Open directory in neotree")
  "d /" '((lambda () (interactive) (dired "/")) :wk "Open /"))

(custom/leader-keys
  "e" '(:ignore t :wk "Eshell/Evaluate")
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate and elisp expression")
  "e h" '(counsel-esh-history :which-key "Eshell history")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region")
  "e R" '(eww-reload :which-key "Reload current page in EWW")
  "e s" '(eshell :which-key "Eshell")
  "e w" '(eww :which-key "EWW emacs web wowser"))

(custom/leader-keys
  "f" '(:ignore t :wk "Files")
  "f c" '((lambda () (interactive)
            (find-file "~/.config/emacs/config.org"))
          :wk "Open emacs config.org")
  "f e" '((lambda () (interactive)
            (dired user-emacs-directory))
          :wk "Open user-emacs-directory in dired")
  "f E" '((lambda () (interactive)
            (dired custom/user-share-emacs-directory))
          :wk "Open custom/user-share-emacs-directory in dired")
  "f d" '(find-grep-dired :wk "Search for string in files in DIR")
  "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
  "f i" '((lambda () (interactive)
            (find-file "~/.config/emacs/init.el"))
          :wk "Open emacs init.el")
  "f r" '(recentf :wk "Find recent files")
  "f u" '(sudo-edit-find-file :wk "Sudo find file")
  "f U" '(sudo-edit :wk "Sudo edit current file"))

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
  "h" '(:ignore t :wk "Help")
  "h a" '(describe-symbol :wk "Apropos")
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
  "h h" '(helpful-at-point :wk "Describe at point")
  "h i" '(info :wk "Info")
  "h I" '(describe-input-method :wk "Describe input method")
  "h k" '(describe-key :wk "Describe key")
  "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
  "h L" '(describe-language-environment :wk "Describe language environment")
  "h m" '(describe-mode :wk "Describe mode")
  "h M" '(describe-keymap :wk "Describe keymap")
  "h p" '(describe-package :wk "Describe package")
  "h r" '(:ignore t :wk "Reload")
  "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config")
  "h r t" '((lambda () (interactive) (load-theme real-theme t)) :wk "Reload theme")
  "h t" '(consult-theme :wk "Load theme")
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
  "m b f" '(org-table-edit-formulas :wk "Edit formulas")
  "m b h" '(org-table-field-info :wk "Field info")
  "m b s" '(org-table-sort-lines :wk "Sort lines")
  "m b r" '(org-table-recalculate :wk "Recalculate")
  "m b R" '(org-table-recalculate-buffer-tables :wk "Recalculate buffer tables")
  "m b d" '(:ignore t :wk "delete")
  "m b d c" '(org-table-delete-column :wk "Delete column")
  "m b d r" '(org-table-kill-row :wk "Delete row")
  "m b i" '(:ignore t :wk "insert")
  "m b i c" '(org-table-insert-column :wk "Insert column")
  "m b i h" '(org-table-insert-hline :wk "Insert horizontal line")
  "m b i r" '(org-table-insert-row :wk "Insert row")
  "m b i H" '(org-table-hline-and-move :wk "Insert horizontal line and move")
  "m c" '(org-capture :wk "Capture")
  "m d" '(:ignore t :wk "Date/deadline")
  "m d d" '(org-deadline :wk "Org deadline")
  "m d s" '(org-schedule :wk "Org schedule")
  "m d t" '(org-time-stamp :wk "Org time stamp")
  "m d T" '(org-time-stamp-inactive :wk "Org time stamp inactive")
  "m e" '(org-export-dispatch :wk "Org export dispatch")
  "m f" '(:ignore t :wk "Fonts")
  "m f b" '((lambda () (interactive) (org-emphasize ?*)) :wk "Bold in region")
  "m f c" '((lambda () (interactive) (org-emphasize ?~)) :wk "Code in region")
  "m f C" '((lambda () (interactive) (org-emphasize ?=)) :wk "Verbatim in region")
  "m f i" '((lambda () (interactive) (org-emphasize ?/)) :wk "Italic in region")
  "m f l" '((lambda () (interactive) (org-emphasize ?$)) :wk "Latex in region")
  "m f u" '((lambda () (interactive) (org-emphasize ?_)) :wk "Underline in region")
  "m f -" '((lambda () (interactive) (org-emphasize ?+)) :wk "Strike through in region")
  "m i" '(org-toggle-item :wk "Org toggle item")
  "m I" '(:ignore t :wk "IDs")
  "m I c" '(org-id-get-create :wk "Create ID")
  "m l" '(:ignore t :wk "Link")
  "m l l" '(org-insert-link :wk "Insert link")
  "m l i" '(org-roam-node-insert :wk "Insert roam link")
  "m p" '(:ignore t :wk "Priority")
  "m p d" '(org-priority-down :wk "Down")
  "m p p" '(org-priority :wk "Set priority")
  "m p u" '(org-priority-down :wk "Up")
  "m q" '(org-set-tags-command :wk "Set tag")
  "m s" '(:ignore t :wk "Tree/Subtree")
  "m s a" '(org-toggle-archive-tag :wk "Archive tag")
  "m s b" '(org-tree-to-indirect-buffer :wk "Tree to indirect buffer")
  "m s c" '(org-clone-subtree-with-time-shift :wk "Clone subtree with time shift")
  "m s d" '(org-cut-subtree :wk "Cut subtree")
  "m s h" '(org-promote-subtree :wk "Promote subtree")
  "m s j" '(org-move-subtree-down :wk "Move subtree down")
  "m s k" '(org-move-subtree-up :wk "Move subtree up")
  "m s l" '(org-demote-subtree :wk "Demote subtree")
  "m s n" '(org-narrow-to-subtree :wk "Narrow to subtree")
  "m s r" '(org-refile :wk "Refile")
  "m s s" '(org-sparse-tree :wk "Sparse tree")
  "m s A" '(org-archive-subtree :wk "Archive subtree")
  "m s N" '(widen :wk "Widen")
  "m s S" '(org-sort :wk "Sort")
  "m t" '(org-todo :wk "Org todo")
  "m B" '(org-babel-tangle :wk "Org babel tangle")
  "m T" '(org-todo-list :wk "Org todo list"))

(custom/leader-keys
  "n" '(:ignore t :wk "Notes")
  ;; "n d o" '(custom/org-notes-dired :wk "Open notes in Dired")
  ;; "n d r" '(custom/org-roam-notes-dired :wk "Open roam notes in Dired")
  ;; "n o" '(:ignore t :wk "Obsidian")
  ;; "n o c" '(obsidian-capture :wk "Create note")
  ;; "n o d" '((lambda () (interactive) (dired obsidian-directory)) :wk "Open notes in Dired")
  ;; "n o f" '(obsidian-tag-find :wk "Find by tag")
  ;; "n o j" '(obsidian-jump :wk "Jump to note")
  ;; "n o m" '(obsidian-move-file :wk "Move note/file")
  ;; "n o r" '(obsidian-update :wk "Update")
  ;; "n o /" '(obsidian-search :wk "Search")
  ;; "n o ?" '(obsidian-hydra/body :wk "Everything")
  ;; "n" '(:ignore t :wk "Org Roam")
  "n a" '(:ignore t :wk "Alias")
  "n a a" '(org-roam-alias-add :wk "Add alias")
  "n a r" '(org-roam-alias-remove :wk "Remove alias")
  "n d" '(:ignore t :wk "Roam dailies")
  "n d c" '(org-roam-dailies-capture-today :wk "Cature today")
  "n d t" '(org-roam-dailies-goto-today :wk "Go to today")
  "n d j" '(org-roam-dailies-goto-next-note :wk "Next note")
  "n d k" '(org-roam-dailies-goto-previous-note :wk "Previous note")
  "n D" '(custom/org-roam-notes-dired :wk "Open notes in Dired")
  "n f" '(org-roam-node-find :wk "Find note")
  "n i" '(org-roam-node-insert :wk "Insert note")
  "n l" '(org-roam-buffer-toggle :wk "Toggle note buffer")
  "n r" '(:ignore t :wk "References")
  "n r" '(org-roam-ref-add :wk "Add reference")
  "n R" '(org-roam-ref-remove :wk "Remove reference")
  "n t" '(org-roam-tag-add :wk "Add tag")
  "n T" '(org-roam-tag-remove :wk "Remove tag")
)

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
  "t d" '(toggle-debug-on-error :wk "Debug on error")
  "t e" '(eshell-toggle :wk "Eshell")
  "t f" '(flycheck-mode :wk "Flycheck")
  "t i" '(imenu-list-smart-toggle :wk "Imenu list")
  "t l" '(display-line-numbers-mode :wk "Line numbers")
  "t n" '(neotree-toggle :wk "Neotree")
  "t r" '(rainbow-mode :wk "Rainbow mode")
  "t t" '(visual-line-mode :wk "Word Wrap")
  "t v" '(vterm :wk "Vterm")
  "t z" '(writeroom-mode :wk "Zen mode"))

(custom/leader-keys
  "W" '(custom/hydra-window/body :wk "Windows hydra")
  ;; Window splits
  "w" '(:ingore t :wk "Windows")
  "w c" '(evil-window-delete :wk "Close window")
  "w n" '(evil-window-new :wk "New window")
  "w q" '(:ingore t :wk "Close on side")
  "w q h" '(custom/evil-close-left-window :wk "Left")
  "w q j" '(custom/evil-close-down-window :wk "Down")
  "w q k" '(custom/evil-close-up-window :wk "Up")
  "w q l" '(custom/evil-close-right-window :wk "Right")
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

(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "C--" 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(defun custom/pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(evil-scroll-up
                   evil-scroll-down
                   evil-window-right
                   evil-window-left
                   evil-window-up
                   evil-window-down
                   scroll-up-command
                   scroll-down-command
                       tab-select
                       tab-next))
  (advice-add command :after #'custom/pulse-line))

(set-face-attribute 'default nil
  :font "JetBrainsMono NFM"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :family "Ubuntu Nerd Font"
  :height 100
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :family "JetBrainsMono NFM Mono"
  :height 80
  :weight 'medium)
(set-face-attribute 'fixed-pitch-serif nil
  :inherit 'fixed-pitch
  :slant 'italic)

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
;; (set-face-attribute 'font-lock-keyword-face nil
;;   :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right, idk why
;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono NFM-9"))

;; Uncomment the following line if line spacing needs adjusting.
;; (setq-default line-spacing 0.12)

(use-package ligature
  :after prog-mode
  :config
    (ligature-set-ligatures 't '("www"))
    ;; Enable ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
    (global-ligature-mode 1))

(unless (custom/termux-p)
  (use-package mixed-pitch
    :hook (org-mode . mixed-pitch-mode)
    :diminish
    :config
    (dolist (faces '(;; org-level-1
                     ;; org-level-2
                     ;; org-level-3
                     ;; org-level-4
                     ;; org-level-5
                     ;; org-level-6
                     ;; org-level-7
                     ;; org-level-8
                           org-modern-label
                     org-property-value
                     org-special-keyword
                     org-drawer
                     org-document-face))
      (add-to-list 'mixed-pitch-fixed-pitch-faces faces)))
      ;; (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-modern-tag)
      ;; (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-property-value)
      ;; (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-special-keyword)
      ;; (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-drawer)
)

(use-package hl-todo
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

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . #'nerd-icons-completion-marginalia-setup)
  :config
    (nerd-icons-completion-mode))

(use-package doom-modeline
  :demand
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-battery t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package doom-themes
  ;; :demand
  :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    ;;(doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

(unless (custom/termux-p)
  (use-package ewal-doom-themes :demand)
  (use-package ewal
    :demand
    :config
      (set-face-attribute 'line-number-current-line nil
        :foreground (ewal-load-color 'comment)
        :inherit 'default)
      (set-face-attribute 'line-number nil
        :foreground (ewal--get-base-color 'green)
        :inherit 'default))
)

(defvar real-theme nil
  "It represents theme to load at startup.\nIt will be loaded st startup with `load-theme' and restarted with SPC-h-r-t.")

(if (custom/termux-p)
    (setq real-theme 'doom-dracula) ;; for termux
  (setq real-theme 'ewal-doom-one)) ;; for PC

(load-theme real-theme t)

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(use-package company
  :after prog-mode
  :diminish
  :custom
    (company-begin-commands '(self-insert-command))
    (company-idle-delay .1)
    (company-minimum-prefix-length 2)
    (company-show-numbers t)
    (company-tooltip-align-annotations 't)
    (global-company-mode t)
  :config
    (add-hook 'prog-mode-hook (lambda ()
                                (setq-local company-idle-delay 0
                                            company-selection-wrap-around t
                                            company-minimum-prefix-length 1))))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package vertico
  :defer 2
  :bind (:map vertico-map
    ("C-j" . vertico-next)
    ("C-k" . vertico-previous)
    ("C-l" . vertico-exit))
  :custom (enable-recursive-minibuffers t)
  :config (vertico-mode))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package vertico-prescient
  :after vertico
  :config
    (prescient-persist-mode 1)
    (vertico-prescient-mode))

(use-package orderless
  :after vertico
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom (marginalia--pangram "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
  :init (marginalia-mode))

(unless (custom/termux-p)
  (use-package dashboard
    ;; :demand
    :hook (dashboard-mode . (lambda () (with-current-buffer "*dashboard*" (emacs-lock-mode 'kill))))
    :custom
      (initial-buffer-choice (lambda () (dashboard-open)))
      (dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))
      (dashboard-banner-logo-title
"You still refuse to accept my god-hood?
Keep your own god!
In fact, this might be a good time to pray to him.
For I beheld Satan as he fell FROM HEAVEN! LIKE LIGHTNING!")
      (dashboard-center-content t)
      (dashboard-agenda-prefix-format " %i %s ")
      (dashboard-items '((recents  . 5)))
                         ;; (bookmarks . 5)
                         ;; (projects . 5)
                         ;; (agenda . 5)
                         ;; (registers . 5)
    :config
      (dashboard-setup-startup-hook)
      (evil-collection-dashboard-setup)
      (evil-collection-define-key 'normal 'dashboard-mode-map
        "j" 'widget-forward
        "k" 'widget-backward
        "l" 'dashboard-return))
)

(unless (custom/termux-p)
  (use-package dirvish
    :init (dirvish-override-dired-mode t) ; dirvish takes over dired
    :custom
      (dirvish-cache-dir (expand-file-name "dirvish" custom/user-share-emacs-directory))
      (dirvish-attributes '(collapse git-msg file-time file-size))
      (dirvish-default-layout '(1 0.15 0.5))
    :config
      (evil-collection-define-key 'normal 'dirvish-mode-map
        "p" 'dirvish-yank-menu
        "q" 'dirvish-quit)
      (dirvish-define-preview eza (file)
        "Use `eza' to generate directory preview."
        :require ("eza") ; tell Dirvish to check if we have the executable
        (when (file-directory-p file) ; we only interest in directories here
          `(shell . ("eza" "-al" "--color=always" "--icons"
                     "--group-directories-first" ,file))))
      (add-to-list 'dirvish-preview-dispatchers 'eza)
      ;; lines not wrapping
      (add-hook 'dirvish-find-entry-hook
          (lambda (&rest _) (setq-local truncate-lines t))))
      ;; rebinds all dired commands to dirvish
      ;; with dirvish-override-dired-mode it already moved dired commands to dirvish
      ;; but it didn't toggle the dirvish window layout
      ;; (defalias 'dired 'dirvish))
)

(use-package dired
  :ensure nil
  :init
    (evil-collection-dired-setup)
  :custom
    (insert-directory-program "ls")
    (dired-listing-switches "-Hl --almost-all --group-directories-first")
    (dired-kill-when-opening-new-dired-buffer t)
    (image-dired-dir (expand-file-name "image-dired" custom/user-share-emacs-directory))
  :config
    (defun custom/dired-go-to-home ()
      (interactive)
      "Spawns `dired' in user's home directory."
      (dired "~/"))
    (evil-collection-define-key 'normal 'dired-mode-map
      [remap evil-yank] 'dired-ranger-copy
      "gh" 'custom/dired-go-to-home
      "p"  'dired-ranger-paste
      "h"  'dired-up-directory
      "l"  'dired-find-file))

;; (use-package dired-open
;;   :after dired
;;   :config
;;     (setq dired-open-extensions '(("gif" . "swaiymg")
;;                                   ("jpg" . "swaiymg")
;;                                   ("png" . "swaiymg")
;;                                   ("mkv" . "mpv")
;;                                   ("mp4" . "mpv"))))

(use-package diredfl
  :after dired
  :hook
    ((dired-mode . diredfl-mode)
     ;; highlight parent and directory preview as well
     (dirvish-directory-view-mode . diredfl-mode))
  :config
    (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dired-ranger
  :after dired
  :config
    (evil-collection-define-key 'normal 'dired-mode-map
      [remap evil-yank] 'dired-ranger-copy
      "p" 'dired-ranger-paste))

(use-package helpful
  :bind
    ([remap describe-function] . helpful-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-symbol] . helpful-symbol)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key))

(unless (custom/termux-p)
  (use-package which-key
    :diminish
    :defer 5
    :custom
      (which-key-side-window-location 'bottom)
      (which-key-sort-order #'which-key-key-order-alpha)
      (which-key-sort-uppercase-first nil)
      (which-key-add-column-padding 1)
      (which-key-max-display-columns nil)
      (which-key-min-display-lines 6)
      (which-key-max-description-length nil)
      (which-key-allow-imprecise-window-fit nil)
      (which-key-separator "  ")
      (which-key-idle-delay 0.5)
    :config
      (which-key-mode 1))
)

(unless (custom/termux-p)
  (use-package elfeed
    :custom
      (elfeed-feeds  '("https://sachachua.com/blog/feed/"))
      (elfeed-search-filter "@6-months-ago"))
)

(unless (custom/termux-p)

(use-package minesweeper
  :config
    (evil-set-initial-state 'minesweeper-mode 'emacs))

(use-package tetris
  :ensure nil
  :config
    (evil-set-initial-state 'tetris-mode 'insert))
)

(use-package magit
  :custom
    (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
    (magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package git-timemachine
  :after git-timemachine ;; I don't know why it's loading after itself
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map
      (kbd "C-j") 'git-timemachine-show-previous-revision
      (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package imenu-list
  :custom
    (imenu-list-focus-after-activation t)
    (imenu-list-auto-resize t)
  :config
    (evil-collection-imenu-list-setup)
    (evil-define-key 'normal imenu-list-major-mode-map
      "j" 'forward-button
      "k" 'backward-button))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))

(use-package company-org-block
  :after org
  :custom
    (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
    (org-appear-trigger 'manual)
    (org-appear-autolinks t)
  :config
    (add-hook 'org-mode-hook (lambda ()
      (add-hook 'evil-insert-state-entry-hook
        #'org-appear-manual-start
        nil
        t)
      (add-hook 'evil-insert-state-exit-hook
        #'org-appear-manual-stop
          nil
          t))))

(use-package org-auto-tangle
  :after org
  :diminish
  :hook (org-mode . org-auto-tangle-mode))

(unless (custom/termux-p)
  (use-package org-modern
    :after org
    ;; :init (add-hook 'org-mode-hook 'org-modern-mode t)
    :hook (org-mode . org-modern-mode)
    :custom-face
      ;; (org-modern-label ((t (:height 1.2))))
    :custom
      (org-modern-star nil)
      (org-modern-list nil)
      (org-modern-table nil))

(use-package org-modern-indent
    :after org
    :quelpa (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    :init (add-hook 'org-mode-indent-hook #'org-modern-indent-mode))
)

(use-package org-roam
  :after org
  :init
    (setq org-roam-v2-ack t)
    (if (custom/termux-p)
        (setq org-roam-directory "~/storage/shared/org-roam")
      (setq org-roam-directory "~/org-roam"))
  :custom
    (org-roam-db-location (expand-file-name "org/org-roam.db" custom/user-share-emacs-directory))
    (org-roam-dailies-directory "journals/")
    (org-roam-node-display-template (concat "${title} " (propertize "${tags}" 'face 'org-tag)))
    (org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+date: %U\n")
         :unnarrowed t)
        ("g" "video game" plain "%?"
         :target (file+head "games/${slug}.org"
                            "#+title: ${title}\n#+filetags: :games:\n#+date: %U\n#+TODO: DROPPED(d) ENDLESS(e) UNFINISHED(u) UNPLAYED(U) TODO(t) | BEATEN(b) COMPLETED(c) MASTERED(m)\n* Status\n* Notes")
         :unnarrowed t)
        ("b" "book" plain "%?"
         :target (file+head "books/${slug}.org"
                            "#+title: ${title}\n#+filetags: :books:\n#+date: %U\n#+todo: DROPPED(d) UNFINISHED(u) UNREAD(U) TODO(t) | READ(r)\n* Status\n* Notes")
         :unnarrowed t)))
    (org-roam-dailies-capture-templates
     '(("d" "default" entry "* %?" :target
        (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :dailie:\n"))))
  :config
    (org-roam-setup)
    (evil-collection-org-roam-setup)
    (require 'org-roam-export)
    ;; if the file is dailie then increase buffer's size automatically
    (require 'org-roam-dailies)
    (add-hook 'org-roam-dailies-find-file-hook (lambda () (text-scale-set 3))))

(use-package org-roam-ui)

(unless (custom/termux-p)
  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
      (org-superstar-remove-leading-stars t)
      (org-superstar-item-bullet-alist
        '((?+ . ?✸)
          (?* . ?•)
          (?- . ?●))))
)

(unless (custom/termux-p)
  (use-package org-yt
    :after org
    :quelpa (org-yt :fetcher github :repo "TobiasZawada/org-yt")
    :config
      (require 'org-yt)

      (defun custom/org-image-link (protocol link _description)
        "Interpret LINK as base64-encoded image data."
        (cl-assert (string-match "\\`img" protocol) nil
                   "Expected protocol type starting with img")
        (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
          (cl-assert buf nil
                     "Download of image \"%s\" failed." link)
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n")
            (buffer-substring-no-properties (point) (point-max)))))

      (org-link-set-parameters
       "imghttp"
       :image-data-fun #'custom/org-image-link)

      (org-link-set-parameters
       "imghttps"
       :image-data-fun #'custom/org-image-link))
)

(use-package toc-org
  :after org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(defun custom/org-notes-dired ()
  "Opens org-directory in Dired."
  (interactive)
  (dired org-directory))

(defun custom/org-roam-notes-dired ()
  "Opens org-roam-directory in Dired."
  (interactive)
  (dired org-roam-directory))

(defun custom/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file."
  (interactive)
  (org-map-entries 'org-id-get-create))

(use-package org
  :hook
    (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'custom/org-resize-latex-overlays nil t)))
    ;; after refiling and archiving tasks agenda files aren't saved, I fix that
    (org-after-refile-insert . (lambda () (save-some-buffers '('org-agenda-files))))
    (org-archive . (lambda () (save-some-buffers '('org-agenda-files))))
  :bind
    ([remap org-return] . custom/org-good-return)
  :custom-face
    ;; setting size of headers
    (org-document-title ((nil (:inherit outline-1 :height 1.7))))
    (org-level-1 ((nil (:inherit outline-1 :height 1.2))))
    (org-level-2 ((nil (:inherit outline-2 :height 1.2))))
    (org-level-3 ((nil (:inherit outline-3 :height 1.2))))
    (org-level-4 ((nil (:inherit outline-4 :height 1.2))))
    (org-level-5 ((nil (:inherit outline-5 :height 1.2))))
    (org-level-6 ((nil (:inherit outline-6 :height 1.2))))
    (org-level-7 ((nil (:inherit outline-7 :height 1.2))))
    (org-list-dt ((nil (:weight bold))))
    (org-agenda-date-today ((nil (:height 1.3))))
    (org-ellipsis ((nil (:underline t))))
  :custom
    (org-directory org-roam-directory)
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
    (org-capture-templates
     '(("t" "Todo" entry (file "inbox.org")
        "* TODO %?\n %a")))
    ;; =========== org agenda ===========
    (org-agenda-files (list (expand-file-name "agenda.org" org-roam-directory)(expand-file-name "inbox.org" org-roam-directory)))
    (org-agenda-prefix-format ;; format at which tasks are displayed
     '((agenda . " %i ")
       (todo . " %i ")
       (tags . "%c %-12:c")
       (search . "%c %-12:c")))
    (org-agenda-category-icon-alist ;; icons for categories
     `(("tech" ,(list (nerd-icons-mdicon "nf-md-laptop" :height 1.5)) nil nil :ascent center)
       ("school" ,(list (nerd-icons-mdicon "nf-md-school" :height 1.5)) nil nil :ascent center)
       ("personal" ,(list (nerd-icons-mdicon "nf-md-drama_masks" :height 1.5)) nil nil :ascent center)
       ("content" ,(list (nerd-icons-faicon "nf-fae-popcorn" :height 1.5)) nil nil :ascent center)))
    (org-agenda-include-all-todo nil)
    (org-agenda-start-day "+0d")
    (org-agenda-span 3)
    (org-agenda-hide-tags-regexp ".*")
    (org-agenda-skip-scheduled-if-done t)
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-skip-timestamp-if-done t)
    (org-agenda-columns-add-appointments-to-effort-sum t)
    ;; (org-agenda-custom-commands nil)
    (org-agenda-default-appointment-duration 60)
    (org-agenda-mouse-1-follows-link t)
    (org-agenda-skip-unavailable-files t)
    (org-agenda-use-time-grid nil)
    (org-agenda-block-separator 8411)
    (org-agenda-window-setup 'current-window)
    (org-refile-targets '((org-agenda-files :maxlevel . 1)))
    (org-refile-use-outline-path nil)
    (org-archive-location (expand-file-name "agenda-archive.org::" org-roam-directory))
    (org-hide-emphasis-markers t)
    (org-hide-leading-stars t)
    (org-html-validation-link nil)
    (org-pretty-entities t)
    (org-image-actual-width nil)
    (org-startup-with-inline-images t)
    (org-startup-indented t) ;; use org-indent-mode at startup
    (org-cycle-inline-images-display t)
    (org-cycle-separator-lines 0)
    (org-display-remote-inline-images 'download)
    (org-list-allow-alphabetical t)
    (org-log-into-drawer t) ;; time tamps from headers and etc. get put into :LOGBOOK: drawer
    (org-fontify-quote-and-verse-blocks t)
    (org-preview-latex-image-directory (expand-file-name "org/lateximg/" custom/user-share-emacs-directory))
    (org-preview-latex-default-process 'dvisvgm)
    (org-latex-to-html-convert-command "latexmlc \\='literal:%i\\=' --profile=math --preload=siunitx.sty 2>/dev/null")
    (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    (org-id-locations-file (expand-file-name "org/.org-id-locations" custom/user-share-emacs-directory))
    (org-return-follows-link t)
    (org-blank-before-new-entry nil) ;; no blank lines when doing M-return
    (org-M-RET-may-split-line nil)
    (org-insert-heading-respect-content t)
    (org-tags-column 0)
    (org-babel-load-languages '((emacs-lisp . t) (shell . t) (C . t)))
    (org-confirm-babel-evaluate nil)
    (org-edit-src-content-indentation 0)
    (org-src-preserve-indentation t)
    (org-export-preserve-breaks t)
    (org-export-allow-bind-keywords t)
    (org-export-with-toc nil)
    (org-export-with-smart-quotes t)
    (org-export-backends '(ascii html icalendar latex odt md))
    ;; (org-export-with-properties t)
    (org-startup-folded t)
    ;; (org-ellipsis "󱞣")
  :config
    (add-to-list 'display-buffer-alist
                 '("*Agenda Commands*"
                   (display-buffer-at-bottom)
                   (window-height . 12)))
    (add-to-list 'display-buffer-alist
                 '("*Org Select*"
                   (display-buffer-at-bottom)
                   (window-height . 12)))
    (add-to-list 'display-buffer-alist
                 '("*Org Links*"
                   (display-buffer-at-bottom)
                   (window-height . 1)))
    (add-to-list 'display-buffer-alist
                 '("*Org Babel Results*"
                   (display-buffer-at-bottom)))

    (defun custom/org-resize-latex-overlays ()
      "It rescales all latex preview fragments correctly with the text size as you zoom text. It's fast, since no image regeneration is required."
      (cl-loop for o in (car (overlay-lists))
               if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
               do (plist-put (cdr (overlay-get o 'display))
                             :scale (expt text-scale-mode-step
                                          text-scale-mode-amount))))
    (plist-put org-format-latex-options :foreground nil)
    (plist-put org-format-latex-options :background nil)

    ;; evil keybindings
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map (kbd "SPC") nil)
      (define-key evil-motion-state-map (kbd "RET") nil)
      (define-key evil-motion-state-map (kbd "TAB") nil)
      (evil-define-key 'normal org-mode-map
        "gj" 'evil-next-visual-line
        "gk" 'evil-previous-visual-line
        (kbd "C-j") 'org-next-visible-heading
        (kbd "C-k") 'org-previous-visible-heading
        (kbd "C-S-J") 'org-forward-heading-same-level
        (kbd "C-S-K") 'org-backward-heading-same-level
        (kbd "M-h") 'org-metaleft
        (kbd "M-j") 'org-metadown
        (kbd "M-k") 'org-metaup
        (kbd "M-l") 'org-metaright
        (kbd "M-H") 'org-shiftmetaleft
        (kbd "M-J") 'org-shiftmetadown
        (kbd "M-K") 'org-shiftmetaup
        (kbd "M-L") 'org-shiftmetaright
        (kbd "M-<return>") 'org-meta-return))

    ;; In tables pressing RET doesn't follow links.
    ;; I fix that
    (defun custom/org-good-return ()
      "`org-return' that allows for following links in table."
      (interactive)
      (if (org-at-table-p)
          (if (org-in-regexp org-link-any-re 1)
              (org-open-at-point)
            (org-return))
        (org-return)))
    ;; saving agenda files after changing TODO state in org-agenda
    ;; (advice-add 'org-agenda-todo :after (lambda () (save-some-buffers (list org-agenda-files))))
    ;; (advice-add 'org-agenda-todo :after
    ;;         (lambda (&rest args)
    ;;           (save-some-buffers (list org-agenda-files))
    ;;           (apply #'org-agenda-todo args)))
    (advice-add 'org-agenda-todo :after
            (lambda (&rest _)
              (when (called-interactively-p 'any)
                (save-some-buffers (list org-agenda-files)))))

)

;; it's for html source block syntax highlighting
(use-package htmlize)

(use-package smartparens
  :hook (prog-mode) ;; add `smartparens-mode` to these hooks
  :config (require 'smartparens-config)) ;; load default config
(use-package evil-smartparens :after smartparens)

(unless (custom/termux-p)
  (use-package projectile
    :diminish projectile-mode
    :custom
      (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" custom/user-share-emacs-directory))
      (projectile-switch-project-action #'projectile-dired)
    :config (projectile-mode)
    :bind-keymap
      ("C-c p" . projectile-command-map))

  (use-package counsel-projectile
    :after projectile
    :config (counsel-projectile-mode 1))
)

(unless (custom/termux-p)

(use-package compile
  :custom
    (compilation-scroll-output t)
  :config
    (add-to-list 'display-buffer-alist
                 '("*compilation*"
                   (display-buffer-at-bottom)
                   (window-height . 12)))
    (add-to-list 'display-buffer-alist
                 '("*Compile-log*"
                   (display-buffer-at-bottom)
                   (window-height . 12)))
    (defadvice compile (before ad-compile-smart activate)
      "Advises `compile' so it sets the argument COMINT to t."
      (ad-set-arg 1 t))
    (defadvice compile (after ad-compile-smart activate)
      "Advises `compile' so it moves to the compilation buffer."
      (switch-to-buffer-other-window "*compilation*"))
    (defadvice recompile (after compile-command activate)
      "Advises `recompile' so it moves to the compilation buffer."
      (switch-to-buffer-other-window "*compilation*"))

    (evil-set-initial-state 'compilation-mode 'normal)
    (evil-set-initial-state 'comint-mode 'normal)

    (evil-define-key 'normal comint-mode-map (kbd "q") 'quit-window)
)


(dolist (buffer '("*Messages*"
                  "*Backtrace*"
                  "*Warnings*"
                  "*Async Shell Command*"))
  (add-to-list 'display-buffer-alist
               `(,buffer
                 (display-buffer-at-bottom)
                 (window-height . 12))))

(defadvice async-shell-command (after shell-command activate)
  "Advises `async-shell-command' to move to it's buffer after activation,
set its' evil state to normal and to bind 'q' to `quit-window'"
  (switch-to-buffer-other-window "*Async Shell Command*")
  (evil-change-state 'normal)
  (evil-local-set-key 'normal (kbd "q") 'quit-window))

(use-package flycheck
  :defer 1
  :after prog-mode
  :diminish
  :init (global-flycheck-mode))

(use-package eglot
  :ensure nil
  :after prog-mode
  :custom (eglot-autoshutdown t))

(use-package flycheck-eglot
  :after eglot)

(dolist (mode '(css-ts-mode-hook
                python-ts-mode-hook
                bash-ts-mode-hook
                c++-ts-mode-hook
                mhtml-mode-hook))
  (add-hook mode 'eglot-ensure))

(use-package lua-mode)
(use-package nix-mode)

(add-hook 'bash-ts-mode-hook (lambda () (setq-local compile-command (concat "bash " (buffer-name)))))

(add-hook 'c++-ts-mode-hook (lambda () (setq-local compile-command (concat "g++ " (buffer-name) " -o " (file-name-sans-extension (buffer-name)) " && ./" (file-name-sans-extension (buffer-name))))))

(defalias 'elisp-mode 'emacs-lisp-mode)

(use-package bug-hunter)

(add-hook 'python-ts-mode-hook (lambda () (setq-local compile-command (concat "python " (buffer-name)))))

(use-package lorem-ipsum
  :custom (lorem-ipsum-sentence-separator " "))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
     ;; (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     ;; (go "https://github.com/tree-sitter/tree-sitter-go")
     ;; (html "https://github.com/tree-sitter/tree-sitter-html")
     ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     ;; (json "https://github.com/tree-sitter/tree-sitter-json")
     ;; (make "https://github.com/alemuller/tree-sitter-make")
     ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")))
     ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
     ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(unless (treesit-language-available-p 'bash)
  (message "Installing tree-sitter parsers")
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(setq major-mode-remap-alist
 '((c-or-c++-mode . c-or-c++-ts-mode)
   (c++-mode . c++-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (sh-mode . bash-ts-mode)))

(use-package autoinsert
  :hook (prog-mode . auto-insert-mode)
  :custom
    (auto-insert-directory (expand-file-name "templates/" user-emacs-directory))
    (auto-insert-query nil)
  :config
    (add-to-list 'auto-insert-alist '(bash-ts-mode nil "#!/usr/bin/env bash\n\n"))
    (add-to-list 'auto-insert-alist '(python-ts-mode nil "#!/usr/bin/env python\n\n"))
    (add-to-list 'auto-insert-alist '(c++-ts-mode . "cpp.cpp")))

(use-package yasnippet
  :after eglot
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; This is for html snippets
;; (use-package emmet-mode
;;   :defer t
;;   :after html-mode mhtml-mode
;;   :config
;;     (evil-collection-define-key 'normal 'html-mode-map
;;       "TAB" 'emmet-expand-line)
;;     (evil-collection-define-key 'normal 'mhtml-mode-map
;;       "TAB" 'emmet-expand-line))

)

(unless (custom/termux-p)
  (use-package company-shell
    :after sh-mode
    :custom
      (add-to-list 'company-backends 'company-shell)
      (add-to-list 'company-backends 'company-shell-env))
)

(use-package eshell
  :custom
    (eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
    (eshell-rc-script (expand-file-name "profile" eshell-directory-name))    ;; your profile for eshell; like a bashrc for eshell
    (eshell-aliases-file (expand-file-name "aliases" eshell-directory-name)) ;; sets an aliases file for the eshell
    (eshell-history-file-name (expand-file-name "eshell-history" custom/user-share-emacs-directory))
    (eshell-last-dir-ring-file-name (expand-file-name "eshell-lastdir" custom/user-share-emacs-directory))
    (eshell-history-size 5000)
    (eshell-buffer-maximum-lines 5000)
    (eshell-hist-ignoredups t)
    (eshell-scroll-to-bottom-on-input nil)
    (eshell-destroy-buffer-when-process-dies t)
  :config
    (evil-define-key 'insert 'eshell-mode-map (kbd "C-d") 'eshell-life-is-too-much)
    (eat-eshell-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
    (eshell-syntax-highlighting-global-mode +1))

(use-package eat
  :after eshell)

(unless (custom/termux-p)
  (use-package vterm
    :config
      (setq shell-file-name "/bin/bash"
            vterm-max-scrollback 5000))
)

(use-package sudo-edit)

(use-package tab-bar
  :init (tab-bar-mode 1)
  :custom
    (tab-bar-show 1)                                      ;; hide bar if <= 1 tabs open
    (tab-bar-close-button-show nil)                       ;; hide tab close / X button
    (tab-bar-new-tab-choice (lambda () (dashboard-open))) ;; buffer to show in new tabs
    (tab-bar-tab-hints t)                                 ;; show tab numbers
)

(use-package buffer-move)

(defun custom/evil-close-down-window ()
  "Goes down the window and closes it"
  (interactive)
  (evil-window-down 1)
  (evil-window-delete))

(defun custom/evil-close-up-window ()
  "Goes up the window and closes it"
  (interactive)
  (evil-window-up 1)
  (evil-window-delete))

(defun custom/evil-close-left-window ()
  "Goes left the window and closes it"
  (interactive)
  (evil-window-left 1)
  (evil-window-delete))

(defun custom/evil-close-right-window ()
  "Goes right the window and closes it"
  (interactive)
  (evil-window-right 1)
  (evil-window-delete))

(use-package windresize)
(use-package hydra)
;; All-in-one window managment. Makes use of some custom functions,
;; `ace-window' (for swapping), `windmove' (could probably be replaced
;; by evil?) and `windresize'.
;; inspired by https://github.com/jakebox/jake-emacs/blob/main/jake-emacs/init.org#hydra

(defhydra custom/hydra-window (:hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_h_          _/_ vertical      _b_uffer        _<left>_  
_l_          _-_ horizontal    _f_ind file     _<down>_  
_k_          _m_aximize        _s_wap          _<up>_    
_j_          _c_lose           _[_backward     _<right>_ 
_q_uit        _e_qualize        _]_forward      ^
^             ^               _K_ill            ^
^             ^                 ^               ^
"
   ;; Movement
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)

   ;; Split/manage
   ("-" evil-window-split)
   ("/" evil-window-vsplit)
   ("c" evil-window-delete)
   ("d" evil-window-delete)
   ("m" delete-other-windows)
   ("e" balance-windows)

   ;; Window switching
   ("H" buf-move-left)
   ("J" buf-move-down)
   ("K" buf-move-up)
   ("L" buf-move-right)

   ;; Switch
   ("b" counsel-ibuffer)
   ("f" counsel-switch-buffersel-find-file)
   ("P" project-find-file)
   ("s" ace-swap-window)
   ("[" previous-buffer)
   ("]" next-buffer)
   ("K" kill-this-buffer)

   ;; Resize
   ("<left>" windresize-left)
   ("<right>" windresize-right)
   ("<down>" windresize-down)
   ("<up>" windresize-up)

   ("q" nil))

(unless (custom/termux-p)
  (use-package writeroom-mode)
)
