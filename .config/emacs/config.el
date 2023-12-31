(scroll-bar-mode -1)                 ; Disable visible scrollbar
(tool-bar-mode -1)                   ; Disable the toolbar
(tooltip-mode -1)                    ; Disable tooltips
(menu-bar-mode -1)                   ; Disable the menu bar
(set-fringe-mode 10)                 ; Give some breathing room
(global-auto-revert-mode t)          ; Automatically show changes if the file has changed
(global-visual-line-mode t)          ; Enable truncated lines (line wrapping)
(global-display-line-numbers-mode t) ; Line numbers
(delete-selection-mode 1)            ; You can select text and delete it by typing (in emacs keybindings).
(electric-pair-mode 0)               ; Turns off automatic parens pairing
(electric-indent-mode -1)            ; Turn off the weird indenting that Emacs does by default.
(column-number-mode 1)               ; Column number in modeline
(fset 'yes-or-no-p 'y-or-n-p)        ; Simplyfying yes or no prompts
(save-place-mode 1)                  ; Saving last place in file
(set-default-coding-systems 'utf-8)  ; Setting default conding to utf-8
(display-battery-mode 1)             ; Setting battery percentage in modeline
(indent-tabs-mode 0)                 ; Using spaces instead of tabs for indentation

;; This folder is for everything that clutters user-emacs-directory
(defvar user-share-emacs-directory "~/.local/share/emacs/"
  "Elisp packages cache folders/files normally clutter user-emacs-directory.
The same goes for some default files like bookmarks file.
In order to prevent that this variable exists.
Most of the stuff will get redirected here.")

(setq-default visible-bell nil ;; Set up the visible bell
              inhibit-startup-message nil ; default emacs startup message
              custom-file (concat user-share-emacs-directory "custom.el") ; custom settings that emacs autosets put into it's own file
              backup-directory-alist '((".*" . "~/.local/share/Trash/files")) ; moving backup files to trash directory
              recentf-save-file (concat user-share-emacs-directory "recentf") ; recentf file put somewhere else
              recentf-max-saved-items nil ; infinite amount of entries in recentf file
              recentf-auto-cleanup 'never ; not cleaning recentf file
              bookmark-default-file (concat user-share-emacs-directory "bookmarks") ; bookmarks file put somewhere else
              elfeed-db-directory (concat user-share-emacs-directory "elfeed") ; elfeed cache? directory
              auto-save-list-file-prefix (concat user-share-emacs-directory "auto-save-list/.saves-")
              ;; auto-save-list-file-name (concat user-share-emacs-directory "auto-save-list/list")
              prescient-save-file (concat user-share-emacs-directory "var/prescient-save.el")
              global-auto-revert-non-file-buffers t ; refreshing buffers when files have changed
              use-dialog-box nil ; turns off graphical dialog boxes
              tramp-persistency-file-name (concat user-share-emacs-directory "tramp") ; tramp file put somewhere else
              save-place-file (concat user-share-emacs-directory "places")
              url-configuration-directory (concat user-share-emacs-directory "url") ; cache from urls (eww)
              multisession-directory (concat user-share-emacs-directory "multisession")
              transient-history-file (concat user-share-emacs-directory "transient/history.el")
              initial-major-mode 'fundamental-mode ; setting scratch buffer in fundamental mode
              initial-scratch-message nil ; deleting scratch buffer message
              scroll-conservatively 1000 ; Scroll one line at a time
              scroll-margin 1 ; Keep a margin of 1 line when scrolling at the window's edge
              tab-always-indent nil
              vc-follow-symlinks t ; Enable follow symlinks
              indent-tabs-mode nil ; use spaces instead of tabs for indenting
              standard-indent 2) ; indenting set to 2

;; turn off line numbers in certain modes
(dolist (mode '(neotree-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                Info-mode-hook
                helpful-mode-hook
                help-mode-hook
                dashboard-mode-hook
                dashboard-after-initialize-hook
                dired-mode-hook
                org-agenda-mode-hook
                which-key-mode-hook
                tldr-mode-hook
                dictionary-mode-hook
                Man-mode-hook
                woman-mode-hook
                ibuffer-mode-hook
                elisp-refs-mode-hook
                imenu-list-minor-mode-hook
                imenu-list-major-mode-hook
                imenu-list-after-jump-hook
                imenu-list-update-hook
                backtrace-revert-hook
                backtrace-mode-hook
                calendar-mode-hook
                special-mode-hook
                outline-mode-hook
                eat-mode-hook
                compilation-mode-hook
                Custom-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun quit-window (&optional kill window)
  "Quit WINDOW, deleting it, and kill its buffer.
WINDOW must be a live window and defaults to the selected one.
The buffer is killed instead of being buried.
This function ignores the information stored in WINDOW's `quit-restore' window parameter."
  (interactive "P")
  (set-window-parameter window 'quit-restore `(frame frame nil ,(current-buffer)))
  (quit-restore-window window 'kill))

;; Some file extensions set for certain modes
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . conf-colon-mode))

;; locking buffers from killing
(with-current-buffer "*scratch*"
          (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
          (emacs-lock-mode 'kill))

;; Make ESC quit prompts immediately
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; make utf-8 the coding system
(prefer-coding-system 'utf-8)

;; Initialize package sources
(require 'package)

(setq package-user-dir (concat user-share-emacs-directory "packages/")
      package-gnupghome-dir (concat user-share-emacs-directory "gpg")
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
      use-package-always-defer t) ; packages by default will be lazy loaded, like the will have defer: t

(use-package quelpa
  :demand
  :custom
    (quelpa-dir (concat user-share-emacs-directory "quelpa/"))
    (quelpa-checkout-melpa-p nil))
    ;; (quelpa-build-dir (concat quelpa-dir "build/"))
    ;; (quelpa-melpa-dir (concat quelpa-dir "melpa/"))
    ;; (quelpa-packages-dir (concat quelpa-dir "packages/")))
(use-package quelpa-use-package
  :demand
  :after quelpa)

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
  :demand
  :init
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
          evil-want-C-u-scroll t
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  :bind
    (:map evil-normal-state-map
      ([remap evil-search-forward] . 'swiper))
  :config
    (evil-mode)
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
    "u" '(universal-argument :wk "Universal argument")
    "x" '(execute-extended-command :wk "M-x"))

  (custom/leader-keys
    "TAB" '(:ignore t :wk "Spacing/Indent")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "TAB SPC" '(untabify :wk "Untabify")
    "TAB DEL" '(whitespace-cleanup :wk "Clean whitespace"))

  (custom/leader-keys
    "RET" '(bookmark-jump :wk "Go to bookmark"))

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
    "b b" '(counsel-ibuffer :wk "Switch to buffer")
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
    "d h" '((lambda () (interactive) (dired "~/")) :wk "Open home directory")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired")
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
              (dired user-share-emacs-directory))
            :wk "Open user-share-emacs-directory in dired")
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
    "m f b" '((lambda () (interactive) (custom/org-format-in-region "*")) :wk "Bold in region")
    "m f c" '((lambda () (interactive) (custom/org-format-in-region "~")) :wk "Code in region")
    "m f C" '((lambda () (interactive) (custom/org-format-in-region "=")) :wk "Verbatim in region")
    "m f i" '((lambda () (interactive) (custom/org-format-in-region "/")) :wk "Italic in region")
    "m f l" '((lambda () (interactive) (custom/org-format-in-region "$")) :wk "Latex in region")
    "m f u" '((lambda () (interactive) (custom/org-format-in-region "_")) :wk "Underline in region")
    "m f -" '((lambda () (interactive) (custom/org-format-in-region "+")) :wk "Strike through in region")
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
    "M" '(:ignore t :wk "MarkDown")
    "M f" '(:ignore t :wk "Fonts")
    "M f b" '(markdown-insert-bold :wk "Bold in region")
    "M l" '(:ignore t :wk "Link")
    "M l l" '(markdown-insert-link :wk "Insert link"))

  (custom/leader-keys
    "n" '(:ignore t :wk "Notes")
    "n d" '(:ignore t :wk "Dired")
    "n d o" '(custom/org-notes-dired :wk "Open notes in Dired")
    "n d r" '(custom/org-roam-notes-dired :wk "Open roam notes in Dired")
    "n o" '(:ignore t :wk "Obsidian")
    "n o c" '(obsidian-capture :wk "Create note")
    "n o d" '((lambda () (interactive) (dired obsidian-directory)) :wk "Open notes in Dired")
    "n o f" '(obsidian-tag-find :wk "Find by tag")
    "n o j" '(obsidian-jump :wk "Jump to note")
    "n o m" '(obsidian-move-file :wk "Move note/file")
    "n o r" '(obsidian-update :wk "Update")
    "n o /" '(obsidian-search :wk "Search")
    "n o ?" '(obsidian-hydra/body :wk "Everything")
    "n r" '(:ignore t :wk "Org Roam")
    "n r a" '(:ignore t :wk "Alias")
    "n r a a" '(org-roam-alias-add :wk "Add alias")
    "n r a r" '(org-roam-alias-remove :wk "Remove alias")
    "n r d" '(:ignore t :wk "Roam dailies")
    "n r d c" '(org-roam-dailies-capture-today :wk "Cature today")
    "n r d t" '(org-roam-dailies-goto-today :wk "Go to today")
    "n r d j" '(org-roam-dailies-goto-next-note :wk "Next note")
    "n r d k" '(org-roam-dailies-goto-previous-note :wk "Previous note")
    "n r f" '(org-roam-node-find :wk "Find note")
    "n r i" '(org-roam-node-insert :wk "Insert note")
    "n r l" '(org-roam-buffer-toggle :wk "Toggle note buffer")
    "n r r" '(:ignore t :wk "References")
    "n r r a" '(org-roam-ref-add :wk "Add reference")
    "n r r r" '(org-roam-ref-remove :wk "Remove reference"))

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
    "t v" '(vterm-toggle :wk "Vterm")
    "t z" '(writeroom-mode :wk "Zen mode"))

  (custom/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w q" '(:ingore t :wk "Close on side")
    "w q h" '(custom/close-left-window :wk "Left")
    "w q j" '(custom/close-down-window :wk "Down")
    "w q k" '(custom/close-up-window :wk "Up")
    "w q l" '(custom/close-right-window :wk "Right")
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

(use-package nerd-icons :defer t)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package all-the-icons-ibuffer
  :after ibuffer
  :hook (ibuffer-mode . (lambda () (all-the-icons-ibuffer-mode t))))

(use-package all-the-icons-ivy-rich
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1))

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
  :demand
  :custom
    (initial-buffer-choice (lambda () (dashboard-open)))
    (dashboard-startup-banner "~/.cache/wal/emacs.svg")
    (dashboard-banner-logo-title "Welcome to Church of Emacs!")
    (dashboard-center-content t)
    (dashboard-items '((recents  . 5)
                       (bookmarks . 5)
                       (projects . 5)
                       (agenda . 5)))
                       ;; (registers . 5)
  :config
    (dashboard-setup-startup-hook)
    (evil-collection-define-key 'normal 'dashboard-mode-map
      "j" 'widget-forward
      "k" 'widget-backward
      "l" 'dashboard-return)
  :bind
    (:map dashboard-mode-map
      ([remap dashboard-next-line] . 'widget-forward)
      ([remap dashboard-previous-line] . 'widget-backward)))

(use-package dired
  :ensure nil
  :init
    (evil-collection-dired-setup)
  :custom
    (insert-directory-program "ls")
    (dired-listing-switches "-lah --group-directories-first")
    (dired-kill-when-opening-new-dired-buffer t)
  :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file))

(use-package dired-open
  :after dired
  :config
    (setq dired-open-extensions '(("gif" . "swaiymg")
                                  ("jpg" . "swaiymg")
                                  ("png" . "swaiymg")
                                  ("mkv" . "mpv")
                                  ("mp4" . "mpv"))))

(use-package diredfl
  :after dired)

(use-package dired-ranger
  :after dired
  :config
    (evil-collection-define-key 'normal 'dired-mode-map
      [remap evil-yank] 'dired-ranger-copy
      "p" 'dired-ranger-paste))

;; (use-package dirvish
;;   :config
;;   (dirvish-override-dired-mode))

(use-package helpful
  :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
  :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

(use-package tldr)

(use-package doom-modeline
  :demand
  :init (doom-modeline-mode 1)
  :custom
    (doom-modeline-battery t))

(use-package elfeed
  :custom
    (elfeed-feeds  '("https://sachachua.com/blog/feed/"))
    (elfeed-search-filter "@6-months-ago"))

(set-face-attribute 'default nil
  :font "JetBrainsMono NFM"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :family "Ubuntu Nerd Font"
  ;; :font "GoMono Nerd Font"
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
    (global-ligature-mode 't))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
    (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-property-value)
    (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-special-keyword)
    (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-drawer))

(use-package minesweeper
  :config
    (evil-set-initial-state 'minesweeper-mode 'emacs))

(use-package magit
  :defer t
  :custom
    (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
    (magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package imenu-list
  :custom
    (imenu-list-focus-after-activation t
     imenu-list-auto-resize t))

(use-package ivy
  :demand
  :diminish
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
  :custom
    (ivy-use-virtual-buffers t
     ivy-count-format "(%d/%d) "
     enable-recursive-minibuffers t)
  :config
    (ivy-mode))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
    (ivy-virtual-abbreviate 'full
     ivy-rich-switch-buffer-align-virtual-buffer t
     ivy-rich-path-style 'abbrev)
  :config
    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer))

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

(use-package ivy-prescient
  :after counsel
  :custom
    (ivy-prescient-enable-filtering nil)
    ;; Here are commands that I don't want to get sorted
    (ivy-prescient-sort-commands '(:not counsel-recentf swiper swiper-isearch ivy-switch-buffer))
  :config
    (prescient-persist-mode 1)
    (ivy-prescient-mode 1))

(use-package swiper
  :demand)

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

(use-package lorem-ipsum)

(use-package neotree
  :custom
    (neo-smart-open t)
    (neo-show-hidden-files t)
    (neo-window-width 35)
    (neo-window-fixed-size nil)
    (inhibit-compacting-font-caches t)
    (projectile-switch-project-action 'neotree-projectile-action)
  :config
    ;; truncate long file names in neotree
    (add-hook 'neo-after-create-hook
          #'(lambda (_)
              (with-current-buffer (get-buffer neo-buffer-name)
                (setq truncate-lines t)
                (setq word-wrap nil)
                (make-local-variable 'auto-hscroll-mode)
                (setq auto-hscroll-mode nil)))))

(use-package org-roam
  :demand
  :after org
  :init
    (setq org-roam-v2-ack t
          org-roam-directory "~/org-roam")
  :custom
    (org-roam-db-location (concat user-share-emacs-directory "org/org-roam.db"))
    (org-roam-dailies-directory "journals/")
    (org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+date: %U\n")
         :unnarrowed t)))
  :bind
    (("C-c n l" . org-roam-buffer-toggle)
     ("C-c n f" . org-roam-node-find)
     ("C-c n i" . org-roam-node-insert))
  :config
    (org-roam-setup)
    (evil-collection-org-roam-setup)
    (require 'org-roam-export))

;; (use-package org-roam-ui
;;   :defer t
;;   :after org-roam)

;; (use-package simple-httpd
;;   :defer t
;;   :after org-roam-ui)
;; (use-package websocket
;;   :defer t
;;   :after org-roam-ui)
;; (use-package f
;;   :defer t
;;   :after org-roam-ui)

(use-package org
  :hook
    (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'custom/org-resize-latex-overlays nil t)))
    (org-mode . (lambda () (org-indent-mode t)))
  :bind
    ([remap org-insert-heading-respect-content] . org-meta-return)
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
    (org-agenda-date-today ((t (:height 1.3))))
  :custom
    (org-directory "~/org/")
    (org-agenda-files (list (concat org-roam-directory "/agenda.org")(concat org-roam-directory "/nonagenda.org")(concat org-roam-directory "/phone.org")))
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
     '(("t" "Todo" entry (file "~/org-roam/nonagenda.org")
        "* TODO %?\n %a")
       ("T" "Repetable Todo" entry (file "~/org-roam/agenda.org")
        "* TODO %?\n %a")
       ("s" "School Todo" entry (file "~/org-roam/nonagenda.org")
        "* TODO %? :school:\n %i")))
    (org-agenda-include-all-todo nil)
    (org-agenda-skip-scheduled-if-done t)
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-columns-add-appointments-to-effort-sum t)
    (org-agenda-custom-commands nil)
    (org-agenda-default-appointment-duration 60)
    (org-agenda-mouse-1-follows-link t)
    (org-agenda-skip-unavailable-files t)
    (org-agenda-use-time-grid t)
    (org-insert-heading-respect-content nil)
    (org-hide-emphasis-markers t)
    (org-hide-leading-stars t)
    (org-hide-emphasis-markers t)
    (org-pretty-entities t)
    (org-startup-with-inline-images t)
    (org-cycle-inline-images-display t)
    (org-display-remote-inline-images 'download)
    (org-image-actual-width nil)
    (org-list-allow-alphabetical t)
    (org-ellipsis " •")
    (org-agenda-window-setup 'current-window)
    (org-fontify-quote-and-verse-blocks t)
    (org-agenda-block-separator 8411)
    (org-preview-latex-image-directory (concat user-share-emacs-directory "org/lateximg/"))
    (org-preview-latex-default-process 'dvisvgm)
    (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    (org-return-follows-link t)
    (org-id-locations-file (concat user-share-emacs-directory "org/.org-id-locations"))
    (org-export-backends (quote (ascii html icalendar latex odt md)))
    (org-tags-column 0)
    (org-babel-load-languages '((emacs-lisp . t) (shell . t)))
    (org-confirm-babel-evaluate nil)
    (org-edit-src-content-indentation 0)
    (org-export-preserve-breaks t)
    (org-export-with-properties t)
    (org-startup-folded 'overview)
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

     ;; My attempt to create new time keyword STARTED
     ;; which would signify the time at which somehting was started
     ;; (defvar org-started-string "STARTED:"
     ;;   "String to mark started entries.")
     ;; (defconst org-element-started-keyword "STARTED:"
     ;;   "Keyword used to mark started TODO entries.")
     ;; (defconst org-started-time-regexp
     ;;   (concat "\\<" org-started-string " *\\[\\([^]]+\\)\\]")
     ;;   "Matches the STARTED keyword together with a time stamp.")
     ;; (defcustom org-started-keep-when-no-todo nil
     ;;   "Remove STARTED: time-stamp when switching back to a non-todo state?"
     ;;   :group 'org-todo
     ;;   :group 'org-keywords
     ;;   :version "24.4"
     ;;   :package-version '(Org . "8.0")
     ;;   :type 'boolean)
     ;; (defconst org-all-time-keywords
     ;;   (mapcar (lambda (w) (substring w 0 -1))
     ;;           (list org-scheduled-string org-deadline-string
     ;;                 org-clock-string org-closed-string org-started-string))
     ;;   "List of time keywords.")
     ;; (defconst org-keyword-time-regexp
     ;;   (concat "\\<"
     ;;           (regexp-opt
     ;;            (list org-scheduled-string org-deadline-string org-closed-string
     ;;                  org-clock-string org-started-string)
     ;;            t)
     ;;           " *[[<]\\([^]>]+\\)[]>]")
     ;;   "Matches any of the 5 keywords, together with the time stamp.")

    (defun custom/org-resize-latex-overlays ()
      "It rescales all latex preview fragments correctly with the text size as you zoom text. It's fast, since no image regeneration is required."
      (cl-loop for o in (car (overlay-lists))
               if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
               do (plist-put (cdr (overlay-get o 'display))
                             :scale (expt text-scale-mode-step
                                          text-scale-mode-amount))))
    (plist-put org-format-latex-options :foreground nil)
    (plist-put org-format-latex-options :background nil)

    (defvar custom/org-bold-symbol "*"
      "Default symbol for `custom/org-format-in-region' function.")

    (defun custom/org-format-in-region (&optional symbol)
      "Add symbols before and after the selected text."
      (interactive)
      (setq symbol (or symbol
                       (read-string "Enter symbol: " custom/org-bold-symbol)))
      (when (region-active-p)
        (save-excursion
          (goto-char (region-end))
          (insert symbol)
          (goto-char (region-beginning))
          (insert symbol)))
      (deactivate-mark)))

(defun custom/org-insert-heading-or-item-and-switch-to-insert-state-advice (orig-func &rest args)
  "Advice function to run org-insert-heading-respect-content or org-ctrl-c-ret and switch to insert state in the background."
  (let ((result (apply orig-func args)))
    (when (and (evil-normal-state-p) (derived-mode-p 'org-mode))
      (evil-insert-state))
    result))

(advice-add 'org-insert-heading-respect-content :around #'custom/org-insert-heading-or-item-and-switch-to-insert-state-advice)
(advice-add 'org-ctrl-c-ret :around #'custom/org-insert-heading-or-item-and-switch-to-insert-state-advice)

(use-package evil-org
  :after org
  :init
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map (kbd "SPC") nil)
      (define-key evil-motion-state-map (kbd "RET") nil)
      (define-key evil-motion-state-map (kbd "TAB") nil)
      (evil-define-key 'normal org-mode-map (kbd "g j") 'evil-next-visual-line)
      (evil-define-key 'normal org-mode-map (kbd "g k") 'evil-previous-visual-line)
      (evil-define-key 'normal 'org-mode-map (kbd "M-h") 'org-metaleft)
      (evil-define-key 'normal 'org-mode-map (kbd "M-j") 'org-metadown)
      (evil-define-key 'normal 'org-mode-map (kbd "M-k") 'org-metaup)
      (evil-define-key 'normal 'org-mode-map (kbd "M-l") 'org-metaright))

    ;; In tables pressing RET doesn't follow links.
    ;; I fix that
    (defun custom/org-return-follow-link ()
      "If point is on a link, open it. Otherwise, insert a newline.\nIt's used only for following links in tables by pressing RET."
      (interactive)
      (if (org-in-regexp org-link-any-re 1)
          (org-open-at-point)
          (org-return)))

    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key (kbd "RET") 'custom/org-return-follow-link)))

    ;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
    ;; Setting RETURN key in org-mode to follow links
    (setq org-return-follows-link t))

;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(require 'org-tempo)

(use-package company-org-block
  :defer t
  :after org
  :custom
    (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package org-appear
  :after org
  :hook (org-mode . (lambda () (org-appear-mode t)))
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

(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode t)))
  :config
    (setq org-superstar-item-bullet-alist
      '((?+ . ?✸)
        (?* . ?•)
        (?- . ?●))))

(use-package org-auto-tangle
  :defer t
  :after org
  :diminish
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-modern
  :defer t
  :after org
  :init (add-hook 'org-mode-hook 'org-modern-mode t)
  :custom-face
    (org-modern-label ((t (:height 1.2))))
  :custom
    (org-modern-star nil)
    (org-modern-list nil)
    (org-modern-table nil))

(use-package org-modern-indent
  :defer t
  :after org
  :quelpa (:fetcher github :repo "jdtsmith/org-modern-indent")
  :init (add-hook 'org-modern-hook #'org-modern-indent-mode t))

(quelpa '(org-yt :fetcher github :repo "TobiasZawada/org-yt"))
(use-package org-yt
  :ensure nil
  :after org
  :config
    (require 'org-yt)

    (defun org-image-link (protocol link _description)
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
     :image-data-fun #'org-image-link)

    (org-link-set-parameters
     "imghttps"
     :image-data-fun #'org-image-link))

(use-package toc-org
  :defer t
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

(use-package smartparens
  :hook (prog-mode) ;; add `smartparens-mode` to these hooks
  :config
    ;; load default config
    (require 'smartparens-config))
(use-package evil-smartparens :after smartparens)

(use-package projectile
  :diminish projectile-mode
  :custom
    (projectile-known-projects-file (concat user-share-emacs-directory "projectile-bookmarks.eld"))
    (projectile-switch-project-action #'projectile-dired)
  :config (projectile-mode)
  :bind-keymap
    ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :config
    (counsel-projectile-mode 1))

(use-package rainbow-delimiters
  :after prog-mode)

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(add-to-list 'display-buffer-alist
             '("*(Backtrace|Compile-log|Messages|Warnings)*"
               (display-buffer-at-bottom)
               (window-height . 12)))
(add-to-list 'display-buffer-alist
             '("*compilation*"
               (display-buffer-at-bottom)
               (window-height . 12)))
(add-to-list 'display-buffer-alist
             '("*Async Shell Command*"
               (display-buffer-at-bottom)
               (window-height . 12)
               (switch-to-buffer-other-window "*Async Shell Command*")))

(evil-set-initial-state 'shell-mode 'normal)
(evil-define-key 'normal shell-mode-map (kbd "q") 'quit-window)

(add-hook 'compilation-mode-hook '(lambda () (switch-to-buffer-other-window "*compilation*")))

(use-package quickrun
  :defer t
  :after prog-mode
  :config
    (evil-define-key 'normal prog-mode-map (kbd "g r") 'quickrun-region)
    (add-to-list 'display-buffer-alist
                 '("*quickrun*"
                   (display-buffer-at-bottom)
                   (window-height . 5))))

(use-package flycheck
  :after prog-mode
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package lua-mode
  :defer t)
(use-package nix-mode
  :defer t)

(defun custom/cpp-makefile ()
  "Checks for `c++-ts-mode'. Then checks for existence of Makefile.
If not then copy c++ makefile and put it in the current directory"
  (interactive)
  (if (eq major-mode 'c++-ts-mode)
    (unless (file-exists-p "./Makefile")
      (copy-file (concat user-emacs-directory "templates/Makefile-cpp") "./Makefile"))))

(add-hook 'find-file-hook 'custom/cpp-makefile)

(defalias 'elisp-mode 'emacs-lisp-mode)

(use-package bug-hunter :defer t)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")))
     ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     ;; (go "https://github.com/tree-sitter/tree-sitter-go")
     ;; (html "https://github.com/tree-sitter/tree-sitter-html")
     ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     ;; (json "https://github.com/tree-sitter/tree-sitter-json")
     ;; (make "https://github.com/alemuller/tree-sitter-make")
     ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     ;; (python "https://github.com/tree-sitter/tree-sitter-python")
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
   (sh-mode . bash-ts-mode)))

(use-package autoinsert
  :hook (after-init . auto-insert-mode)
  :custom
    (auto-insert-directory (concat user-emacs-directory "templates/"))
    (auto-insert-query nil)
  :config
    (add-to-list 'auto-insert-alist '(bash-ts-mode nil "#!/usr/bin/env bash\n\n"))
    (add-to-list 'auto-insert-alist '(c++-ts-mode . "cpp.cpp")))

(use-package yasnippet
  :defer t
  :after prog-mode)

(use-package yasnippet-snippets
  :defer t
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

(use-package company-shell
  :after sh-mode
  :custom
    (add-to-list 'company-backends 'company-shell)
    (add-to-list 'company-backends 'company-shell-env))

;; Moving focus to async-shell-command after executing it ans setting it to normal mode
(add-hook 'shell-mode-hook '(lambda () (switch-to-buffer-other-window "*Async Shell Command*")))

(use-package eshell
  :custom
    (eshell-directory-name "~/.config/eshell/")
    (eshell-rc-script "~/.config/eshell/profile")    ;; your profile for eshell; like a bashrc for eshell.
    (eshell-aliases-file "~/.config/eshell/aliases") ;; sets an aliases file for the eshell.
    (eshell-history-file-name (concat user-share-emacs-directory "eshell-history"))
    (eshell-last-dir-ring-file-name (concat user-share-emacs-directory "eshell-lastdir"))
    (eshell-history-size 5000)
    (eshell-buffer-maximum-lines 5000)
    (eshell-hist-ignoredups t)
    (eshell-scroll-to-bottom-on-input nil)
    (eshell-destroy-buffer-when-process-dies t)
    ;; (eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh" "less")))
    ;; :config
    ;; (evil-set-initial-state 'eshell-mode 'emacs)
  :config
    (eat-eshell-mode))

(use-package eshell-toggle
  :custom
    (eshell-toggle-size-fraction 3)
    (eshell-toggle-use-projectile-root t)
    (eshell-toggle-run-command nil)
    (eshell-toggle-init-function #'eshell-toggle-init-eshell))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
    (eshell-syntax-highlighting-global-mode +1))

(use-package eat
  :defer t
  :after eshell)

(use-package vterm
  :defer t
  :config
    (setq shell-file-name "/bin/bash"
          vterm-max-scrollback 5000))
    ;; (add-hook 'vterm-mode-hook (lambda () (setq evil-default-state 'emacs))))

(use-package vterm-toggle
  :after vterm
  :custom
    (vterm-toggle-fullscreen-p nil)
    (vterm-toggle-scope 'project)
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
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
                  (window-height . 0.4))))

(use-package sudo-edit)

(use-package doom-themes
  :demand
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
(use-package ewal-doom-themes)
(use-package ewal
  :demand
  :config
    (set-face-attribute 'line-number-current-line nil
      :foreground (ewal-load-color 'comment)
      :inherit 'default)
    (set-face-attribute 'line-number nil
      :foreground (ewal--get-base-color 'green)
      :inherit 'default))

(defvar real-theme nil
  "It represents theme to load at startup.\nIt will be loaded st startup with `load-theme' and restarted with SPC-h-r-t.")

(setq real-theme 'ewal-doom-one) ;; NOTE THIS IS WHERE YOU SHOULD SET YOUR THEME
(load-theme real-theme t)

(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth

(use-package which-key
  :diminish
  :demand
  :custom
    (which-key-side-window-location 'bottom)
    (which-key-sort-order #'which-key-key-order-alpha)
    (which-key-sort-uppercase-first nil)
    (which-key-add-column-padding 1)
    (which-key-max-display-columns nil)
    (which-key-min-display-lines 6)
    (which-key-side-window-slot -10)
    (which-key-side-window-max-height 0.25)
    (which-key-idle-delay 0.8)
    (which-key-max-description-length nil)
    (which-key-allow-imprecise-window-fit nil)
    (which-key-separator "  ")
    (which-key-idle-delay 0.5)
  :config
    (which-key-mode 1))

(use-package buffer-move)

(defun custom/close-down-window ()
  "Goes down the window and closes it"
  (interactive)
  (evil-window-down 1)
  (evil-window-delete))

(defun custom/close-up-window ()
  "Goes up the window and closes it"
  (interactive)
  (evil-window-up 1)
  (evil-window-delete))

(defun custom/close-left-window ()
  "Goes left the window and closes it"
  (interactive)
  (evil-window-left 1)
  (evil-window-delete))

(defun custom/close-right-window ()
  "Goes right the window and closes it"
  (interactive)
  (evil-window-right 1)
  (evil-window-delete))

(use-package writeroom-mode)
