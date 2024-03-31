(defun custom/termux-p ()
  "Checks if Emacs is running inside of Termux."
  (and (getenv "TERMUX_VERSION")))

(unless (custom/termux-p)
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1)))         ; Disable visible scrollbar
(unless (custom/termux-p)
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1)))           ; Disable the toolbar
(unless (custom/termux-p)
  (if (fboundp 'set-fringe-mode)
        (set-fringe-mode 10)))       ; Give some breathing room
(tooltip-mode -1)                    ; Disable tooltips
(menu-bar-mode -1)                   ; Disable the menu bar
(global-auto-revert-mode t)          ; Automatically show changes if the file has changed
(global-visual-line-mode t)          ; Enable truncated lines (line wrapping)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; Line numbers in programming modes
(delete-selection-mode 1)            ; You can select text and delete it by typing (in emacs keybindings).
(electric-pair-mode 0)               ; Turns off automatic parens pairing
(electric-indent-mode -1)            ; Turn off the weird default indenting.
(column-number-mode 1)               ; Column number in modeline
(display-battery-mode 1)             ; Setting battery percentage in modeline
;; (indent-tabs-mode 0)                 ; Using spaces instead of tabs for indentation

(defvar custom/user-share-emacs-directory "~/.local/share/emacs/"
  "Directory to redirect cache/dump files.
Elisp packages cache folders/files normally clutter `user-emacs-directory'.
The same goes for some default files like bookmarks file.
In order to prevent that this variable exists.
Most of the stuff will get redirected here.")

(setq-default bookmark-default-file (expand-file-name "bookmarks" custom/user-share-emacs-directory) ; bookmarks file put somewhere else
              auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" custom/user-share-emacs-directory)
              ;; prescient-save-file (expand-file-name "var/prescient-save.el" custom/user-share-emacs-directory)
              custom-file (expand-file-name "custom.el" custom/user-share-emacs-directory) ; custom settings that emacs autosets put into it's own file
              backup-directory-alist '((".*" . "~/.local/share/Trash/files")) ; moving backup files to trash directory
              tramp-persistency-file-name (expand-file-name "tramp" custom/user-share-emacs-directory) ; tramp file put somewhere else
              save-place-file (expand-file-name "places" custom/user-share-emacs-directory)
              url-configuration-directory (expand-file-name "url" custom/user-share-emacs-directory) ; cache from urls (eww)
              multisession-directory (expand-file-name "multisession" custom/user-share-emacs-directory)
              transient-history-file (expand-file-name "transient/history.el" custom/user-share-emacs-directory)
              request-storage-directory (expand-file-name "request" custom/user-share-emacs-directory))

(setq-default visible-bell nil ;; Set up the visible bell
              global-auto-revert-non-file-buffers t ; refreshing buffers when files have changed
              use-dialog-box nil ; turns off graphical dialog boxes
              initial-buffer-choice t ; scratch buffer is the buffer to show at the startup
              initial-major-mode 'fundamental-mode ; setting scratch buffer in `fundamental-mode'
              initial-scratch-message nil ; scratch buffer message
              inhibit-startup-message nil ; default emacs startup message
              scroll-conservatively 1000 ; Scroll one line at a time
              scroll-margin 1 ; Keep a margin of 1 line when scrolling at the window's edge
              vc-follow-symlinks t ; Enable follow symlinks
              tab-always-indent 'complete
              indent-tabs-mode nil ; use spaces instead of tabs for indenting
              standard-indent 2 ; indenting set to 2
              auto-revert-interval 1
              use-short-answers t ; replace yes-no prompts with y-n
              fast-but-imprecise-scrolling t ; fast scrolling
              inhibit-compacting-font-caches t
              sentence-end-double-space nil ; sentences end with 1 space
              create-lockfiles nil) ; no files wiht ".#"

;; showing init time in scratch buffer
(add-hook 'after-init-hook (lambda () (setq initial-scratch-message (concat "Initialization time: " (emacs-init-time)))))

;; this opens links in android's default apps in termux
(if (custom/termux-p)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; Some file extensions set for certain modes
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . conf-colon-mode))

;; locking buffers from killing
(with-current-buffer "*scratch*"
          (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
          (emacs-lock-mode 'kill))

;; Make ESC quit prompts immediately
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-c f c" 'custom/find-config-file)

(defun custom/find-config-file ()
  "Opens config.org file in `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory))
)

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

;; cleaning whistespace when saving file
(add-hook 'before-save-hook #'whitespace-cleanup)

;; returning to normal garbage collection
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(use-package use-package
  :custom
    (use-package-verbose t)
    (use-package-always-ensure t)
    (use-package-always-defer t)) ; packages by default will be lazy loaded, like they will have defer: t

(use-package package
  :custom
    (package-user-dir (expand-file-name "packages/" custom/user-share-emacs-directory))
    (package-gnupghome-dir (expand-file-name "gpg" custom/user-share-emacs-directory))
    (package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("elpa" . "https://elpa.gnu.org/packages/")
                        ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")))
    (package-async t)
  :init
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
)

;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (use-package gcmh
;;   :demand
;;   :diminish
;;   :custom
;;     (gcmh-mode 1)
;;     (gcmh-idle-delay 10)
;;     (gcmh-high-cons-threshold (* 32 1024 1024))
;;     (gc-cons-percentage 0.8))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package meow
  :demand
  :custom
    (meow-use-clipboard t)
    (initial-buffer-choice (lambda () (meow-cheatsheet)))
    (meow-expand-hint-remove-delay 0)
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
     ;; '("TAB" . evilnc-comment-or-uncomment-lines))
     ;; '("f c" . (find-file "~/.config/emacs/config.org"))

    (if (custom/termux-p) (meow-leader-define-key '("s" . save-buffer)))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (meow-setup)
  (meow-global-mode 1)
)

(use-package pulse
  :config
    (defun custom/pulse-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (dolist (command '(meow-beginning-of-thing
                       meow-end-of-thing
                       ;; evil-scroll-up
                       ;; evil-scroll-down
                       ;; evil-window-right
                       ;; evil-window-left
                       ;; evil-window-up
                       ;; evil-window-down
                       scroll-up-command
                       scroll-down-command
                       tab-select
                       tab-next))
      (advice-add command :after #'custom/pulse-line))
)

(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "C--" 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package abbrev
  :ensure nil
  :hook (text-mode . abbrev-mode) ;; `text-mode' is a parent of `org-mode'
  :config
    (define-abbrev global-abbrev-table "btw" "by the way")
    (define-abbrev global-abbrev-table "idk" "I don't know")
    (define-abbrev global-abbrev-table "tbh" "to be honest")
)

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
    (recentf-save-file (expand-file-name "recentf" custom/user-share-emacs-directory)) ; location of the file
    (recentf-max-saved-items nil) ; infinite amount of entries in recentf file
    (recentf-auto-cleanup 'never) ; not cleaning recentf file
  ;; :general
  ;;   (custom/leader-keys
  ;;     "f r" '(recentf :wk "Find recent files"))
)

(use-package save-place
  :ensure nil
  :hook (after-init . save-place-mode)
)

(use-package eww
  :custom (eww-auto-rename-buffer 'title))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative))

(use-package project
  :custom (project-list-file (expand-file-name "projects" custom/user-share-emacs-directory)))

(use-package tab-bar
  :init
    (tab-bar-mode 1)
    (advice-add #'tab-new
                :after
                (lambda (&rest _) (when (y-or-n-p "Rename tab? ")
                                    (call-interactively #'tab-rename))))
  :custom
    (tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
    (tab-bar-close-button-show nil)      ;; hide tab close / X button
    (tab-bar-new-tab-choice "*scratch*") ;; buffer to show in new tabs
    (tab-bar-tab-hints t)                ;; show tab numbers
  ;; :custom-face (tab-bar ((t (:box (:line-width 2 :style flat-button)))))
  :bind (
    ("C-c t TAB" . tab-next)
    ("C-c t T"   . tab-bar-mode)
    ("C-c t 1"   . (lambda () (interactive) (tab-select 1)))
    ("C-c t 2"   . (lambda () (interactive) (tab-select 2)))
    ("C-c t 3"   . (lambda () (interactive) (tab-select 3)))
    ("C-c t 4"   . (lambda () (interactive) (tab-select 4)))
    ("C-c t 5"   . (lambda () (interactive) (tab-select 5)))
    ("C-c t 6"   . (lambda () (interactive) (tab-select 6)))
    ("C-c t 7"   . (lambda () (interactive) (tab-select 7)))
    ("C-c t 8"   . (lambda () (interactive) (tab-select 8)))
    ("C-c t 9"   . (lambda () (interactive) (tab-select 9)))
    ("C-c t 0"   . (lambda () (interactive) (tab-select 0)))
    ("C-c t t"   . tab-new)
    ("C-c t d"   . tab-bar-close-tab)
    ("C-c t r"   . tab-rename)
  )
)

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
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NFM-9"))

;; Uncomment the following line if line spacing needs adjusting.
;; (setq-default line-spacing 0.12)

(use-package ligature
  :after prog-mode
  :hook (prog-mode . ligature-mode)
  :config
    (ligature-set-ligatures 't '("www"))
    ;; Enable ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!" "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||" "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???" "<:<" ";;;")))

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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package all-the-icons-ibuffer
  :after ibuffer
  :hook (ibuffer-mode . (lambda () (all-the-icons-ibuffer-mode t))))

(use-package all-the-icons-completion
  :after marginalia
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :config
    (all-the-icons-completion-mode))

;; (use-package doom-modeline
;;   :demand
;;   :init (doom-modeline-mode 1)
;;   :custom (doom-modeline-battery t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-battery t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode conf-mode
  ;; :general
  ;;   (custom/leader-keys
  ;;     "t r" '(rainbow-mode :wk "Rainbow mode"))
)

(use-package doom-themes
  ;; :demand
  :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    ;; (doom-themes-neotree-config)
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

(defvar custom/real-theme nil
  "It represents theme to load at startup.
It will be loaded st startup with `custom/load-real-theme' and restarted with 'SPC-h-r-t'.")

(defun custom/load-real-theme ()
  "Loads `real-theme'."
  (interactive)
  (load-theme custom/real-theme t))

(if (custom/termux-p)
    (setq custom/real-theme 'doom-dracula) ;; for termux
  (setq custom/real-theme 'ewal-doom-one)) ;; for PC

(custom/load-real-theme)

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(use-package corfu
  :custom
    (corfu-auto t)
    (corfu-auto-prefix 1)
    (corfu-popupinfo-delay nil)
  ;; it doesn't exit when using meow, the fix was taken from https://gitlab.com/daniel.arnqvist/emacs-config/-/blob/master/init.el?ref_type=heads
  :preface
  ;; (defun custom/corfu-cleanup ()
  ;;   "Close corfu popup if it is active."
  ;;   (if corfu-mode (corfu-quit)))
   :hook (;; (meow-insert-exit . custom/corfu-cleanup)
          (prog-mode . corfu-mode)
          (corfu-mode . corfu-popupinfo-mode))
   :bind (:map corfu-map
               ("C-j" . corfu-next)
               ("C-k" . corfu-previous)
               ("ESC" . corfu-quit)))

(use-package nerd-icons-corfu
  :after corfu
  :hook (corfu-mode . (lambda () (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))
)

(use-package vertico
  :defer 2
  :bind (:map vertico-map
    ("C-j" . vertico-next)
    ("C-k" . vertico-previous)
    ("C-l" . vertico-exit)
    ("M-F" . vertico-buffer-mode))
  :custom
    (enable-recursive-minibuffers t)
  :config
    (vertico-mode)
    (vertico-mouse-mode t)
)

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-l" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :after vertico
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init (savehist-mode t)
  :custom (savehist-file (expand-file-name "history" custom/user-share-emacs-directory)))

(use-package consult
  :after vertico
  ;; :init
  ;;   ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;;   ;; Otherwise use the default `completion--in-region' function.
  ;;   (setq completion-in-region-function
  ;;         (lambda (&rest args)
  ;;           (apply (if vertico-mode
  ;;                      #'consult-completion-in-region
  ;;                    #'completion--in-region)
  ;;                  args)))
)

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom (marginalia--pangram "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
  :init (marginalia-mode))

(use-package dired
  :ensure nil
  ;; :init (evil-collection-dired-setup)
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
    ("b" . dired-up-directory))
  :custom
    (insert-directory-program "ls")
    (dired-listing-switches "-Hl --almost-all --group-directories-first")
    (dired-kill-when-opening-new-dired-buffer t)
    (image-dired-dir (expand-file-name "image-dired" custom/user-share-emacs-directory))
    (dired-auto-revert-buffer t)
  :config
    (defun custom/dired-go-to-home ()
      (interactive)
      "Spawns `dired' in user's home directory."
      (dired "~/"))
    ;; (evil-collection-define-key 'normal 'dired-mode-map
    ;;   [remap evil-yank] 'dired-ranger-copy
    ;;   "gh" 'custom/dired-go-to-home
    ;;   "p"  'dired-ranger-paste
    ;;   "h"  'dired-up-directory
    ;;   "l"  'dired-find-file)
  ;; :general
  ;;   (custom/leader-keys
  ;;     "d" '(:ignore t :wk "Dired")
  ;;     "d d" '(dired :wk "Open dired")
  ;;     "d h" '(custom/dired-go-to-home :wk "Open home directory")
  ;;     "d j" '(dired-jump :wk "Dired jump to current")
  ;;     "d n" '(neotree-dir :wk "Open directory in neotree")
  ;;     "d /" '((lambda () (interactive) (dired "/")) :wk "Open /"))
)

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

;; (use-package dired-ranger
;;   :after dired
;;   :config
;;     (evil-collection-define-key 'normal 'dired-mode-map
;;       [remap evil-yank] 'dired-ranger-copy
;;       "p" 'dired-ranger-paste))

(use-package dirvish
  :unless (custom/termux-p)
  :init (dirvish-override-dired-mode t) ; dirvish takes over dired
  :custom
    (dirvish-cache-dir (expand-file-name "dirvish" custom/user-share-emacs-directory))
    (dirvish-attributes '(collapse git-msg file-time file-size))
    (dirvish-default-layout '(1 0.15 0.5))
  :config
    ;; (evil-collection-define-key 'normal 'dirvish-mode-map
    ;;   "p" 'dirvish-yank-menu
    ;;   "q" 'dirvish-quit)
    ;; (dirvish-define-preview eza (file)
    ;;   "Use `eza' to generate directory preview."
    ;;   :require ("eza") ; tell Dirvish to check if we have the executable
    ;;   (when (file-directory-p file) ; we only interest in directories here
    ;;     `(shell . ("eza" "-al" "--color=always" "--icons"
    ;;                "--group-directories-first" ,file))))
    ;; (add-to-list 'dirvish-preview-dispatchers 'eza)
    ;; lines not wrapping
    (add-hook 'dirvish-find-entry-hook
        (lambda (&rest _) (setq-local truncate-lines t)))
    ;; rebinds all dired commands to `dirvish-dwim' so when I only have 1 window dirvish will have 3 pane view
    (defalias 'dired 'dirvish-dwim))

(use-package helpful
  :bind
    ([remap describe-function] . helpful-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-symbol] . helpful-symbol)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key)
)

(use-package which-key
  :unless (custom/termux-p)
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

(use-package elfeed
  :unless (custom/termux-p)
  :custom
    (elfeed-db-directory (expand-file-name "elfeed" custom/user-share-emacs-directory)) ; cache? directory
    (elfeed-feeds  '("https://sachachua.com/blog/feed/"))
    (elfeed-search-filter "@6-months-ago"))

(use-package magit
  :custom
    (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
    (magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine))
)

(use-package org
  :ensure nil
  :hook
    (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'custom/org-resize-latex-overlays nil t)))
    ;; after refiling and archiving tasks agenda files aren't saved, I fix that
    (org-after-refile-insert . (lambda () (save-some-buffers '('org-agenda-files))))
    (org-archive . (lambda () (save-some-buffers '('org-agenda-files))))
    ;; (org-capture-after-finalize . (lambda () (save-some-buffers '('org-agenda-files))))
  :bind
    ([remap org-return] . custom/org-good-return)
    ("C-c n a" . org-agenda)
    ("C-c n c" . org-capture)
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
    ;; (org-ellipsis ((nil (:underline t))))
  :custom
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
    ;; ============ org agenda ============
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
    ;; (org-hide-leading-stars t)
    (org-html-validation-link nil)
    (org-pretty-entities t)
    (org-image-actual-width '(300 600))
    (org-startup-with-inline-images t)
    (org-startup-indented t) ;; use org-indent-mode at startup
    (org-indent-mode-turns-on-hiding-stars nil)
    ;; (org-cycle-inline-images-display t)
    (org-cycle-separator-lines 0)
    (org-display-remote-inline-images 'download)
    (org-list-allow-alphabetical t)
    (org-log-done t)
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
    ;; live latex preview
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
    ;; (require 'evil-org-agenda)
    ;; (evil-org-agenda-set-keys)
    ;; (with-eval-after-load 'evil-maps
    ;;   (define-key evil-motion-state-map (kbd "SPC") nil)
    ;;   (define-key evil-motion-state-map (kbd "RET") nil)
    ;;   (define-key evil-motion-state-map (kbd "TAB") nil)
    ;;   (evil-define-key 'normal org-mode-map
    ;;     "gj" 'evil-next-visual-line
    ;;     "gk" 'evil-previous-visual-line
    ;;     (kbd "C-j") 'org-next-visible-heading
    ;;     (kbd "C-k") 'org-previous-visible-heading
    ;;     (kbd "C-S-J") 'org-forward-heading-same-level
    ;;     (kbd "C-S-K") 'org-backward-heading-same-level
    ;;     (kbd "M-h") 'org-metaleft
    ;;     (kbd "M-j") 'org-metadown
    ;;     (kbd "M-k") 'org-metaup
    ;;     (kbd "M-l") 'org-metaright
    ;;     (kbd "M-H") 'org-shiftmetaleft
    ;;     (kbd "M-J") 'org-shiftmetadown
    ;;     (kbd "M-K") 'org-shiftmetaup
    ;;     (kbd "M-L") 'org-shiftmetaright
    ;;     (kbd "M-<return>") 'org-meta-return))

    ;; meow custom state (taken from https://aatmunbaxi.netlify.app/comp/meow_state_org_speed/)
    (setq meow-org-motion-keymap (make-keymap))
    (meow-define-state org-motion
      "Org-mode structural motion"
      :lighter "[O]"
      :keymap meow-org-motion-keymap)

    (meow-define-keys 'org-motion
      '("<escape>" . meow-normal-mode)
      '("i" . meow-insert-mode)
      '("g" . meow-normal-mode)
      '("u" .  meow-undo)
      ;; Moving between headlines
      '("k" .  org-previous-visible-heading)
      '("j" .  org-next-visible-heading)
      ;; Moving between headings at the same level
      '("p" .  org-backward-heading-same-level)
      '("n" .  org-forward-heading-same-level)
      ;; Moving subtrees themselves
      '("K" .  org-subtree-up)
      '("J" .  org-subtree-down)
      ;; Subtree de/promotion
      '("L" .  org-demote-subtree)
      '("H" .  org-promote-subtree)
      ;; Completion-style search of headings
      '("v" .  consult-org-heading)
      ;; Setting subtree metadata
      '("l" .  org-set-property)
      '("t" .  org-todo)
      '("d" .  org-deadline)
      '("s" .  org-schedule)
      '("e" .  org-set-effort)
      ;; Block navigation
      '("b" .  org-previous-block)
      '("f" .  org-next-block)
      ;; Narrowing/widening
      '("N" .  org-narrow-to-subtree)
      '("W" .  widen))

    (meow-define-keys 'normal
      '("O" . meow-org-motion-mode))

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
    (advice-add 'org-agenda-todo :after
            (lambda (&rest _)
              (when (called-interactively-p 'any)
                (save-some-buffers (list org-agenda-files)))))
  ;; :general
  ;;   (custom/leader-keys
  ;;     "m" '(:ignore t :wk "Org")
  ;;     "m a" '(org-agenda :wk "Org agenda")
  ;;     "m b" '(:ignore t :wk "Tables")
  ;;     "m b -" '(org-table-insert-hline :wk "Insert hline in table")
  ;;     "m b a" '(org-table-align :wk "Align table")
  ;;     "m b b" '(org-table-blank-field :wk "Make blank field")
  ;;     "m b c" '(org-table-create-or-convert-from-region :wk "Create/Convert from region")
  ;;     "m b e" '(org-table-edit-field :wk "Edit field")
  ;;     "m b f" '(org-table-edit-formulas :wk "Edit formulas")
  ;;     "m b h" '(org-table-field-info :wk "Field info")
  ;;     "m b s" '(org-table-sort-lines :wk "Sort lines")
  ;;     "m b r" '(org-table-recalculate :wk "Recalculate")
  ;;     "m b R" '(org-table-recalculate-buffer-tables :wk "Recalculate buffer tables")
  ;;     "m b d" '(:ignore t :wk "delete")
  ;;     "m b d c" '(org-table-delete-column :wk "Delete column")
  ;;     "m b d r" '(org-table-kill-row :wk "Delete row")
  ;;     "m b i" '(:ignore t :wk "insert")
  ;;     "m b i c" '(org-table-insert-column :wk "Insert column")
  ;;     "m b i h" '(org-table-insert-hline :wk "Insert horizontal line")
  ;;     "m b i r" '(org-table-insert-row :wk "Insert row")
  ;;     "m b i H" '(org-table-hline-and-move :wk "Insert horizontal line and move")
  ;;     "m c" '(org-capture :wk "Capture")
  ;;     "m d" '(:ignore t :wk "Date/deadline")
  ;;     "m d d" '(org-deadline :wk "Org deadline")
  ;;     "m d s" '(org-schedule :wk "Org schedule")
  ;;     "m d t" '(org-time-stamp :wk "Org time stamp")
  ;;     "m d T" '(org-time-stamp-inactive :wk "Org time stamp inactive")
  ;;     "m e" '(org-export-dispatch :wk "Org export dispatch")
  ;;     "m f" '(:ignore t :wk "Fonts")
  ;;     "m f b" '((lambda () (interactive) (org-emphasize ?*)) :wk "Bold in region")
  ;;     "m f c" '((lambda () (interactive) (org-emphasize ?~)) :wk "Code in region")
  ;;     "m f C" '((lambda () (interactive) (org-emphasize ?=)) :wk "Verbatim in region")
  ;;     "m f i" '((lambda () (interactive) (org-emphasize ?/)) :wk "Italic in region")
  ;;     "m f l" '((lambda () (interactive) (org-emphasize ?$)) :wk "Latex in region")
  ;;     "m f u" '((lambda () (interactive) (org-emphasize ?_)) :wk "Underline in region")
  ;;     "m f -" '((lambda () (interactive) (org-emphasize ?+)) :wk "Strike through in region")
  ;;     "m i" '(org-toggle-item :wk "Org toggle item")
  ;;     "m I" '(:ignore t :wk "IDs")
  ;;     "m I c" '(org-id-get-create :wk "Create ID")
  ;;     "m l" '(:ignore t :wk "Link")
  ;;     "m l l" '(org-insert-link :wk "Insert link")
  ;;     "m l i" '(org-roam-node-insert :wk "Insert roam link")
  ;;     "m p" '(:ignore t :wk "Priority")
  ;;     "m p d" '(org-priority-down :wk "Down")
  ;;     "m p p" '(org-priority :wk "Set priority")
  ;;     "m p u" '(org-priority-down :wk "Up")
  ;;     "m q" '(org-set-tags-command :wk "Set tag")
  ;;     "m s" '(:ignore t :wk "Tree/Subtree")
  ;;     "m s a" '(org-toggle-archive-tag :wk "Archive tag")
  ;;     "m s b" '(org-tree-to-indirect-buffer :wk "Tree to indirect buffer")
  ;;     "m s c" '(org-clone-subtree-with-time-shift :wk "Clone subtree with time shift")
  ;;     "m s d" '(org-cut-subtree :wk "Cut subtree")
  ;;     "m s h" '(org-promote-subtree :wk "Promote subtree")
  ;;     "m s j" '(org-move-subtree-down :wk "Move subtree down")
  ;;     "m s k" '(org-move-subtree-up :wk "Move subtree up")
  ;;     "m s l" '(org-demote-subtree :wk "Demote subtree")
  ;;     "m s n" '(org-narrow-to-subtree :wk "Narrow to subtree")
  ;;     "m s r" '(org-refile :wk "Refile")
  ;;     "m s s" '(org-sparse-tree :wk "Sparse tree")
  ;;     "m s A" '(org-archive-subtree :wk "Archive subtree")
  ;;     "m s N" '(widen :wk "Widen")
  ;;     "m s S" '(org-sort :wk "Sort")
  ;;     "m t" '(org-todo :wk "Org todo")
  ;;     "m B" '(org-babel-tangle :wk "Org babel tangle")
  ;;     "m T" '(org-todo-list :wk "Org todo list"))
)

;; it's for html source block syntax highlighting
(use-package htmlize)

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
)

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
    (org-appear-trigger 'manual)
    (org-appear-autolinks t)
  :config
  ;;   (add-hook 'org-appear-mode-hook (lambda ()
  ;;     (add-hook 'evil-insert-state-entry-hook
  ;;       #'org-appear-manual-start
  ;;       nil
  ;;       t)
  ;;     (add-hook 'evil-insert-state-exit-hook
  ;;       #'org-appear-manual-stop
  ;;         nil
  ;;        t)))
    (add-hook 'org-appear-mode-hook (lambda ()
      (add-hook 'meow-insert-enter-hook
        #'org-appear-manual-start
        nil
        t)
      (add-hook 'meow-insert-exit-hook
        #'org-appear-manual-stop
          nil
          t)))
)

(use-package org-auto-tangle
  :after org
  :diminish
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  ;; :after org
  :init
    (setq org-roam-v2-ack t)
    (if (custom/termux-p)
        (setq org-roam-directory "~/storage/shared/org-roam")
      (setq org-roam-directory "~/org-roam"))
  :custom
    (org-directory org-roam-directory)
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
                            "#+title: ${title}\n#+filetags: :games:\n#+date: %U\n#+TODO: DROPPED(d) ENDLESS(e) UNFINISHED(u) UNPLAYED(U) TODO(t) | BEATEN(b) COMPLETED(c) MASTERED(m)\n* Status\n| Region | Rating | Ownership | Achievements |\n* Notes")

         :unnarrowed t)
        ("b" "book" plain "%?"
         :target (file+head "books/${slug}.org"
                            "#+title: ${title}\n#+filetags: :books:\n#+date: %U\n#+todo: DROPPED(d) UNFINISHED(u) UNREAD(U) TODO(t) | READ(r)\n* Status\n* Notes")
         :unnarrowed t)
        ("a" "animanga" plain "%?"
         :target (file+head "animan/${slug}.org"
                            "#+title: ${title}\n#+filetags: :animan:\n#+date: %U\n#+TODO: DROPPED(d) UNFINISHED(u) TODO(t) | COMPLETED(c)\n* Anime :anime: \n* Manga :manga:")
         :unnarrowed t)
    ))
    (org-roam-dailies-capture-templates
     '(("d" "default" entry "* %?" :target
        (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :dailie:\n"))))

   :bind (
      ("C-c n A a" . org-roam-alias-add)
      ("C-c n A r" . org-roam-alias-remove)
      ("C-c n d c" . org-roam-dailies-capture-today)
      ("C-c n d t" . org-roam-dailies-goto-today)
      ("C-c n d j" . org-roam-dailies-goto-next-note)
      ("C-c n d k" . org-roam-dailies-goto-previous-note)
      ("C-c n D"   . custom/org-roam-notes-dired)
      ("C-c n f"   . org-roam-node-find)
      ("C-c n i"   . org-roam-node-insert)
      ("C-c n l"   . org-roam-buffer-toggle)
      ("C-c n r"   . org-roam-ref-add)
      ("C-c n R"   . org-roam-ref-remove)
      ("C-c n t"   . org-roam-tag-add)
      ("C-c n T"   . org-roam-tag-remove)
      )
  :config
    (org-roam-setup)
    ;; (evil-collection-org-roam-setup)
    (require 'org-roam-export)
    ;; if the file is dailie then increase buffer's size automatically
    (require 'org-roam-dailies)
    ;; (add-hook 'org-roam-dailies-find-file-hook (lambda () (text-scale-set 3)))
    ;; (add-hook 'find-file-hook (lambda () (if (org-roam-dailies--daily-note-p) (text-scale-set 3))))
  ;; :general
  ;;   (custom/leader-keys
  ;;     "n" '(:ignore t :wk "Notes")
  ;;     "n a" '(:ignore t :wk "Alias")
  ;;     "n a a" '(org-roam-alias-add :wk "Add alias")
  ;;     "n a r" '(org-roam-alias-remove :wk "Remove alias")
  ;;     "n d" '(:ignore t :wk "Roam dailies")
  ;;     "n d c" '(org-roam-dailies-capture-today :wk "Cature today")
  ;;     "n d t" '(org-roam-dailies-goto-today :wk "Go to today")
  ;;     "n d j" '(org-roam-dailies-goto-next-note :wk "Next note")
  ;;     "n d k" '(org-roam-dailies-goto-previous-note :wk "Previous note")
  ;;     "n D" '(custom/org-roam-notes-dired :wk "Open notes in Dired")
  ;;     "n f" '(org-roam-node-find :wk "Find note")
  ;;     "n i" '(org-roam-node-insert :wk "Insert note")
  ;;     "n l" '(org-roam-buffer-toggle :wk "Toggle note buffer")
  ;;     "n r" '(:ignore t :wk "References")
  ;;     "n r" '(org-roam-ref-add :wk "Add reference")
  ;;     "n R" '(org-roam-ref-remove :wk "Remove reference")
  ;;     "n t" '(org-roam-tag-add :wk "Add tag")
  ;;     "n T" '(org-roam-tag-remove :wk "Remove tag")
  ;;   )
)

(use-package org-roam-ui
  :custom (org-roam-ui-sync-theme t))

(use-package org-yt
  :unless (custom/termux-p)
  :after org
  :vc (:fetcher github :repo "TobiasZawada/org-yt")
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

(use-package org-sliced-images
  :after org
  :config
    (defalias 'org-remove-inline-images #'org-sliced-images-remove-inline-images)
    (defalias 'org-toggle-inline-images #'org-sliced-images-toggle-inline-images)
    (defalias 'org-display-inline-images #'org-sliced-images-display-inline-images))

(use-package toc-org
  :after org
  :hook (org-mode . #'toc-org-enable))
  ;; :commands toc-org-enable
  ;; :init (add-hook 'org-mode-hook 'toc-org-enable))

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
  :config (require 'smartparens-config)) ;; load default config
;; (use-package evil-smartparens :after smartparens)

(unless (custom/termux-p)

(use-package compile
  :custom
    (compilation-scroll-output 'first-error)
    (compilation-ask-about-save nil)
    (compilation-always-kill nil)
  :config
    (defadvice compile (before ad-compile-smart activate)
      "Advises `compile' so it sets the argument COMINT to t."
      (ad-set-arg 1 t))
    (defadvice recompile (before ad-recompile-smart activate)
      "Advises `recompile' so it sets the argument COMINT to t."
      (setq compilation-arguments (list compile-command t)))
    ;; (defadvice compile (after ad-compile-smart activate)
    ;;   "Advises `compile' so it moves to the compilation buffer."
    ;;   (switch-to-buffer-other-window "*compilation*"))
    ;; (defadvice recompile (after compile-command activate)
    ;;   "Advises `recompile' so it moves to the compilation buffer."
    ;;   (switch-to-buffer-other-window "*compilation*"))

    ;; (evil-set-initial-state 'compilation-mode 'normal)
    ;; (evil-set-initial-state 'comint-mode 'normal)

    ;; (evil-define-key 'normal comint-mode-map (kbd "q") 'quit-window)
)

;; (defadvice async-shell-command (after shell-command activate)
;;   "Advises `async-shell-command' to:
;; ;; 1. Move to it's buffer after activation,
;; 2. Set its' evil state to normal
;; 3. Bind 'q' to `quit-window'"
;;   ;; (switch-to-buffer-other-window "*Async Shell Command*")
;;   (evil-change-state 'normal)
;;   (evil-local-set-key 'normal (kbd "q") 'quit-window))

(use-package flycheck
  :after prog-mode
  :hook (prog-mode . flycheck-mode))

(use-package lua-mode)
(use-package nix-mode)

(use-package sh-script ;; sh-script is the package that declares redirecting shell mode to treesitter mode
  :hook (bash-ts-mode . (lambda () (setq-local compile-command (concat "bash " (buffer-name)))))
)

(use-package c-ts-mode
  :hook
    (c++-ts-mode . (lambda () (setq-local compile-command (concat "g++ " (buffer-name) " -o " (file-name-sans-extension (buffer-name)) " && ./" (file-name-sans-extension (buffer-name))))))
)

(defalias 'elisp-mode 'emacs-lisp-mode)

(use-package bug-hunter)

(use-package python
  :hook (python-ts-mode . (lambda () (setq-local compile-command (concat "python " (buffer-name)))))
)

(use-package lorem-ipsum
  :custom (lorem-ipsum-sentence-separator " "))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
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

(use-package eshell
  :hook
    (eshell-mode . (lambda () (setq mode-line-format nil)))
  :bind (("C-c s e" . eshell))
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
    ;; (keymap-set eshell-mode-map "C-d" #'eshell-life-is-too-much)
    (defalias 'eshell/clear #'eshell/clear-scrollback)
    (add-to-list 'meow-mode-state-list '(eshell-mode . insert)))

(use-package eshell-syntax-highlighting
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eat
  :after eshell
  :hook (eshell-load . eat-eshell-mode))

(use-package vterm
  :unless (custom/termux-p)
  :hook (vterm-mode . (lambda () (setq mode-line-format nil)))
  :bind (("C-c s v" . vterm))
  :custom
    (shell-file-name "/bin/fish")
    (vterm-max-scrollback 5000)
    (vterm-always-compile-module t)
)

(use-package sudo-edit
  :bind ("C-x C-S-f" . sudo-edit-find-file))

(use-package reverso)

(use-package buffer-move)

(use-package writeroom-mode
  :unless (custom/termux-p))

(defun custom/switch-to-buffer-other-window-for-alist (window)
  "Kind of `switch-to-buffer-other-window' but can be used in `display-buffer-alist' with body-function parameter."
  (select-window window))

(setq display-buffer-alist
      '(
        ("^\\*helpful"
         (display-buffer-at-bottom)
         (window-height . 12)
         (dedicated . t))
        ("\\*Help\\*"
         (display-buffer-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))

        ("\\*Agenda Commands\\*"
         (display-buffer-at-bottom)
         (window-height . 12))
        (" \\*Agenda Commands\\*"
         (display-buffer-at-bottom)
         (window-height . 12))
        ("\\*Org Select\\*"
         (display-buffer-at-bottom)
         (window-height . 12))
        ("\\*Org Links\\*"
         (display-buffer-at-bottom)
         (window-height . 1)
         (window-parameters . ((mode-line-format . none))))
        ("\\*Org todo\\*"
         (display-buffer-at-bottom)
         ;; (window-height . 1)
         (window-parameters . ((mode-line-format . none))))
        ("\\*Agenda Commands\\*"
         (display-buffer-at-bottom)
         (window-parameters . ((mode-line-format . none))))
        ("\\*Org Babel Results\\*"
         (display-buffer-at-bottom))

        ("\\*compilation\\*"
         (display-buffer-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ("\\*Compile-log\\*"
         (display-buffer-at-bottom)
         (window-height . 12)
         (body-function . custom/switch-to-buffer-other-window-for-alist))

        ("\\*Messages\\*"
         (display-buffer-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ("\\*Backtrace\\*"
         (display-buffer-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ("\\*Warnings\\*"
         (display-buffer-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ;; ("\\*Async Shell Command\\*"
        ;;  (display-buffer-at-bottom)
        ;;  (window-height . 12)
        ;;  (dedicated . t)
        ;;  (body-function . custom/switch-to-buffer-other-window-for-alist))
        )

      switch-to-buffer-obey-display-actions t) ; `switch-to-buffer' will respect `display-buffer-alist'
