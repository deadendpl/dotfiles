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
      (set-fringe-mode 10)))         ; Give some breathing room
(tooltip-mode -1)                    ; Disable tooltips
(menu-bar-mode -1)                   ; Disable the menu bar
(global-auto-revert-mode t)          ; Automatically show changes if the file has changed
(global-visual-line-mode t)          ; Enable truncated lines (line wrapping)
(delete-selection-mode 1)            ; You can select text and delete it by typing (in emacs keybindings).
(electric-pair-mode 1)               ; Turns on automatic parens pairing
(electric-indent-mode -1)            ; Turns off the weird default indenting.
(column-number-mode 1)               ; Column number in modeline
(display-battery-mode 1)             ; Setting battery percentage in modeline

(defvar custom/user-share-emacs-directory "~/.local/share/emacs/"
  "Directory to redirect cache/dump files.
Elisp packages cache folders/files normally clutter `user-emacs-directory'.
The same goes for some default files like bookmarks file.
In order to prevent that this variable exists.
Most of the stuff will get redirected here.")

(setq-default bookmark-default-file (expand-file-name "bookmarks" custom/user-share-emacs-directory)
              auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" custom/user-share-emacs-directory)
              custom-file (expand-file-name "custom.el" custom/user-share-emacs-directory) ; custom settings that emacs autosets
              backup-directory-alist '((".*" . "~/.local/share/Trash/files")) ; moving backup files to trash directory
              tramp-persistency-file-name (expand-file-name "tramp" custom/user-share-emacs-directory)
              save-place-file (expand-file-name "places" custom/user-share-emacs-directory)
              url-configuration-directory (expand-file-name "url" custom/user-share-emacs-directory) ; cache from urls (eww)
              multisession-directory (expand-file-name "multisession" custom/user-share-emacs-directory)
              transient-history-file (expand-file-name "transient/history.el" custom/user-share-emacs-directory)
              request-storage-directory (expand-file-name "request" custom/user-share-emacs-directory))

(setq-default global-auto-revert-non-file-buffers t ; refreshing buffers without file associated with them
              use-dialog-box nil ; turns off graphical dialog boxes
              use-file-dialog nil
              initial-buffer-choice t ; scratch buffer as a startup buffer
              initial-major-mode 'fundamental-mode ; setting scratch buffer major mode
              ;; initial-scratch-message nil ; scratch buffer message
              inhibit-startup-message nil ; default emacs startup message
              vc-follow-symlinks t ; follow symlinks
              indent-tabs-mode nil ; use spaces instead of tabs for indenting
              tab-width 4 ; it's set by default to 8
              standard-indent 2 ; indenting set to 2
              auto-revert-interval 1
              use-short-answers t ; replace yes-no prompts with y-n
              fast-but-imprecise-scrolling t ; fast scrolling
              inhibit-compacting-font-caches t
              sentence-end-double-space nil ; sentences end with 1 space
              create-lockfiles nil ; no files with ".#"
              make-backup-files nil
              require-final-newline t
              native-comp-async-report-warnings-errors 'silent
              show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t
              truncate-string-ellipsis "…"
              uniquify-buffer-name-style 'forward
              kill-buffer-delete-auto-save-files t
              frame-resize-pixelwise t
              hscroll-margin 2
              hscroll-step 1
              scroll-conservatively 10
              scroll-margin 0
              scroll-preserve-screen-position t
              auto-window-vscroll nil
              mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
              mouse-wheel-scroll-amount-horizontal 1
              kill-do-not-save-duplicates nil
              comment-empty-lines t
              url-privacy-level 'paranoid
              electric-pair-skip-self nil)

;; showing init time in scratch buffer
(if (custom/termux-p)
    (add-hook 'after-init-hook (lambda () (setq initial-scratch-message (concat "Initialization time: " (emacs-init-time)))))
  (setq initial-scratch-message nil))

;; this opens links in android's default apps in termux
(if (custom/termux-p)
    (setq browse-url-browser-function 'browse-url-xdg-open))

;; Some file extensions set for certain modes
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . js-json-mode))

;; locking buffers from killing
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

(defun custom/find-config-file ()
  "Opens config.org file in `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory))
  )

;; make utf-8 the coding system
(set-language-environment "UTF-8")

(defun make-directory-maybe (filename &optional wildcards)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))
(advice-add 'find-file :before #'make-directory-maybe)

;; cleaning whistespace when saving file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; `conf-mode' is not derived from `prog-mode', so I add its hook manually
(add-hook 'conf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

;; removing warning when using some commands
(setq disabled-command-function nil)

(defun launch-test-emacs ()
  "Launches Emacs that only loads test init file."
  (interactive)
  (start-process "emacs-test" nil "emacs" "-Q" "-l" "~/.config/emacs/test-init.el")
  )

(blink-cursor-mode -1)

(keymap-set prog-mode-map "RET" #'newline-and-indent)

(defun execute-extended-command-other-window (prefixarg &optional command-name typed)
  "Execute `execute-extended-command' in a new window."
  (interactive
   (let ((execute-extended-command--last-typed nil))
     (list current-prefix-arg
           (read-extended-command))))
  (switch-to-buffer-other-window (current-buffer))
  (with-suppressed-warnings ((interactive-only execute-extended-command))
    (execute-extended-command prefixarg command-name typed)))

(keymap-global-set "C-x 4 x" #'execute-extended-command-other-window)

(use-package use-package
  :init (setq use-package-enable-imenu-support t)
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
                      ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
                      ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
  (package-async t)
  :init
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  )

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package meow
  :demand
  :custom
  (meow-use-clipboard t)
  (meow-expand-hint-remove-delay 0) ;; when set to 0, it disables numbers popup
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore)
     '("{" . tab-previous)
     '("}" . tab-next))

    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     '("{" . "H-{")
     '("}" . "H-}")
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
     '("{" . tab-previous)
     '("}" . tab-next)
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
     '("<escape>" . ignore))

    (meow-define-keys 'insert
      '("C-." . meow-keypad))
    )

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
                     windmove-up
                     windmove-down
                     windmove-left
                     windmove-right
                     other-window
                     scroll-up-command
                     scroll-down-command
                     tab-select
                     tab-next
                     tab-previous))
    (advice-add command :after #'custom/pulse-line))
  )

;; Make ESC quit prompts immediately
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-c f c" 'custom/find-config-file)
(keymap-global-set "C-c f ." 'find-file-at-point)
(keymap-global-set "C-x K" 'kill-this-buffer)
;; I don't like default window management keybindings so I set my own
;; They are inspired by Doom Emacs keybindings
(keymap-global-set "C-c w j" 'windmove-down)
(keymap-global-set "C-c w h" 'windmove-left)
(keymap-global-set "C-c w k" 'windmove-up)
(keymap-global-set "C-c w l" 'windmove-right)
(keymap-global-set "C-c w v" 'split-window-right)
(keymap-global-set "C-c w s" 'split-window-below)
(keymap-global-set "C-c w c" 'delete-window)
(keymap-global-set "C-c w w" 'other-window)
(keymap-global-set "C-c w q l" 'windmove-delete-right)
(keymap-global-set "C-c w q h" 'windmove-delete-left)
(keymap-global-set "C-c w q j" 'windmove-delete-down)
(keymap-global-set "C-c w q k" 'windmove-delete-up)
(keymap-global-set "M-/" 'hippie-expand)
;; resizing buffer
(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "C--" 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package abbrev
  :ensure nil
  :hook (text-mode . abbrev-mode) ;; `text-mode' is a parent of `org-mode'
  :bind ("C-x \"" . unexpand-abbrev)
  :config
  (if (custom/termux-p)
      (setq abbrev-file-name "~/storage/shared/Sync/backup/abbrev_defs.el")
    (setq abbrev-file-name "~/Sync/backup/abbrev_defs.el"))
  )

(use-package recentf
  :hook ((after-init . recentf-mode)
         (kill-emacs . #'recentf-save-list))
  :bind (("C-c f r" . recentf-open))
  :custom
  (recentf-save-file (expand-file-name "recentf" custom/user-share-emacs-directory)) ; location of the file
  (recentf-max-saved-items nil) ; infinite amount of entries in recentf file
  )

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

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
  ;; (advice-add #'tab-new
  ;;             :after
  ;;             (lambda (&rest _) (when (yes-or-no-p "Rename tab? ")
  ;;                                 (call-interactively #'tab-rename))))
  :custom-face
  (tab-bar-tab ((nil (:inherit 'highlight :background unspecified :foreground unspecified))))
  :custom
  (tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
  (tab-bar-close-button-show nil)      ;; hide tab close / X button
  (tab-bar-new-button-show nil)        ;; hide tab new / + button
  (tab-bar-tab-hints t)                ;; show tab numbers
  (tab-bar-auto-width-max nil)
  )

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom (ibuffer-default-sorting-mode 'filename/process))

(use-package enlight
  :hook (enlight-mode . (lambda () (with-current-buffer "*enlight*"
                                    (emacs-lock-mode 'kill))))
  :custom
  (initial-buffer-choice #'enlight)
  (tab-bar-new-tab-choice #'enlight) ;; buffer to show in new tabs
  (enlight-content
   (concat
    (propertize "Welcome to the Church of Emacs" 'face 'success)
    "\n"
    (concat "Startup time: " (emacs-init-time))
    "\n"
    (enlight-menu
     '(("Org Mode"
        ("Org-Agenda (current day)" (org-agenda nil "a") "a")
        ("Org-Agenda (all ideas)" (org-todo-list "IDEA") "i")
        ("Org-Roam notes" org-roam-node-find "n")
        ("Org-Roam today daily" org-roam-dailies-goto-today "d"))
       ("Other"
        ("Projects" project-switch-project "p"))
       ("Things to remember"
        ("Instead of holding h/l, use letter finding keybindings"))))))
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
  :unless (custom/termux-p)
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!" "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||" "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???" "<:<" ";;;")))

(use-package hl-todo
  :hook ((org-mode prog-mode) . hl-todo-mode)
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
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  (advice-add #'wdired-change-to-wdired-mode :before
              (lambda ()
                (if nerd-icons-dired-mode
                    (nerd-icons-dired-mode -1))))
  (advice-add #'wdired-finish-edit :after
              (lambda ()
                (unless nerd-icons-dired-mode
                  (nerd-icons-dired-mode 1))))
  (advice-add #'wdired-exit :after
              (lambda ()
                (unless nerd-icons-dired-mode
                  (nerd-icons-dired-mode 1))))
  (advice-add #'wdired-abort-changes :after
              (lambda ()
                (unless nerd-icons-dired-mode
                  (nerd-icons-dired-mode 1))))
  )

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . #'nerd-icons-completion-marginalia-setup))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-battery t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom (rainbow-delimiters-max-face-count 5))

(use-package colorful-mode
  :hook (after-init . global-colorful-mode)
  :custom (global-colorful-modes t))

(use-package doom-themes
  ;; :demand
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing modeline on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(if (custom/termux-p)
    (load-theme 'doom-dracula t) ;; if on termux, use some doom theme
  (use-package ewal-doom-themes
    :demand
    :config
    (set-face-attribute 'line-number-current-line nil
                        :foreground (ewal-load-color 'comment)
                        :inherit 'default)
    (set-face-attribute 'line-number nil
                        :foreground (ewal--get-base-color 'green)
                        :inherit 'default)
    (load-theme 'ewal-doom-one t))
  )

(add-to-list 'default-frame-alist '(alpha-background . 95))

(use-package corfu
  :init (global-corfu-mode t)
  :hook (;; (meow-insert-exit . custom/corfu-cleanup)
         ;; ((prog-mode ielm-mode) . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :custom-face
  (corfu-current ((nil (:inherit 'highlight :background unspecified :foreground unspecified))))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-popupinfo-delay nil)
  (corfu-quit-no-match t)
  (global-corfu-minibuffer nil)
  (tab-always-indent 'complete)
  ;; :preface
  ;; ;; it doesn't exit when using meow, the fix was inspired by https://gitlab.com/daniel.arnqvist/emacs-config/-/blob/master/init.el?ref_type=heads#L147
  ;; (defun custom/corfu-cleanup ()
  ;;   "Close corfu popup if it is active."
  ;;   (if (boundp 'corfu-mode)
  ;;       (if corfu-mode (corfu-quit))))
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-l" . corfu-insert)
              ("<escape>" . corfu-quit))
  :config
  ;; (add-to-list 'meow-mode-state-list '(corfu-mode . insert))
  )

(use-package nerd-icons-corfu
  :hook (corfu-mode . (lambda () (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))
  )

(use-package cape
  :init
  ;; The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
)

(use-package vertico
  :hook (after-init . vertico-mode)
  ;; :defer 1
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-exit)
              )
  :custom
  (enable-recursive-minibuffers t)
  (vertico-multiform-commands
   '((recentf-open (vertico-sort-function . nil)))) ;; `recentf-open' will not have sorted entries
  :config
  (vertico-mode)
  (vertico-mouse-mode t)
  (vertico-multiform-mode)
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
  :custom
  (savehist-file (expand-file-name "history" custom/user-share-emacs-directory))
  (savehist-additional-variables '(comint-input-ring))
  )

(use-package consult
  :init
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  :bind
  ;; ([remap project-find-file] . consult-project-buffer)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
  :custom
  (consult-async-min-input 0)
  :config
  (advice-add 'consult-buffer :around
              (lambda (orig-fun &rest args)
                ;; no live preview as loading org mode takes few seconds
                (let ((consult-preview-key nil))
                  (apply orig-fun args))))
  ;; adding project source
  ;; (push 'consult--source-project-recent-file consult-buffer-sources)
  (push 'consult--source-project-buffer consult-buffer-sources)
  )

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :custom
  (insert-directory-program "ls")
  (dired-listing-switches "-lvXAh --group-directories-first")
  (dired-switches-in-mode-line 0)
  (dired-kill-when-opening-new-dired-buffer t)
  (image-dired-dir (expand-file-name "image-dired" custom/user-share-emacs-directory))
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-vc-rename-file t)
  (dired-guess-shell-alist-user
   (list '("\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|bmp\\|webp\\)$" "xdg-open")
         '("\\.\\(pdf\\|epub\\)$" "xdg-open")
         '("\\.\\(mkv\\|mp4\\)$" "xdg-open")
         ;; everything else
         '("\\..*$" "xdg-open")))
  (dired-dwim-target t)
  )

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
)

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key) ; it doesn't work with meow
  ("C-h C-." . helpful-at-point)
  ("C-h '" . describe-face)
  :custom (helpful-max-buffers nil)
  )

(use-package which-key
  :unless (custom/termux-p)
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
  (elfeed-feeds  '("https://sachachua.com/blog/feed/" "https://planet.emacslife.com/atom.xml"))
  (elfeed-search-filter "@6-months-ago")
  :bind (:map elfeed-search-mode-map
              ("f" . elfeed-search-show-entry)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  ;; (add-to-list 'embark-default-action-overrides '(execute-extended-command . helpful-function))
  )

(use-package embark-consult)

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-repository-directories '(("~/.dotfiles" . 0)
                                  ("~/dev" . 1))))

(use-package org
  :ensure nil
  :bind
  ("C-c n c" . org-capture)
  (:map org-mode-map
        ("C-x n t" . org-toggle-narrow-to-subtree)
        ("C-x n r" . custom/org-reverso-grammar-subtree)
        ([remap imenu] . consult-org-heading))
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
  :custom
  (org-M-RET-may-split-line nil)
  (org-babel-load-languages '((emacs-lisp . t) (shell . t) (C . t)))
  (org-blank-before-new-entry nil) ;; no blank lines when doing M-return
  (org-capture-templates
   '(("t" "Todo" entry (file "agenda/inbox.org")
      "* TODO %?")))
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-display-remote-inline-images 'download)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file (expand-file-name "org/.org-id-locations" custom/user-share-emacs-directory))
  (org-image-actual-width '(300 600))
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-insert-heading-respect-content t)
  (org-link-file-path-type 'relative)
  (org-list-allow-alphabetical t)
  (org-log-done t)
  (org-log-into-drawer t) ;; time tamps from headers and etc. get put into :LOGBOOK: drawer
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-src-preserve-indentation t)
  (org-startup-folded t)
  (org-startup-indented t) ;; use `org-indent-mode' at startup
  (org-startup-with-inline-images t)
  (org-tags-column 0)
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
  :config
  ;; opening video files from links in mpv
  (add-to-list 'org-file-apps '("\\.\\(mp4\\|mkv\\)$" . "mpv %s"))
  ;; unfolding header after `consult-org-heading'
  (advice-add 'consult-org-heading :after #'org-fold-show-entry)
  )

;; it's for html source block syntax highlighting
(use-package htmlize)

(use-package org
  :bind ([remap org-return] . custom/org-good-return)
  :config
  (defun custom/org-good-return ()
    "`org-return' that allows for following links in table."
    (interactive)
    (if (and (org-at-table-p) (org-in-regexp org-link-any-re 1))
        (org-open-at-point)
      (org-return))
    ))

(use-package org
  :config
  (advice-add 'org-meta-return :around (lambda (orig-fun &rest args)
                                         (if (or (org-at-item-checkbox-p)
                                                 (ignore-errors (org-entry-is-todo-p)))
                                             (org-insert-todo-heading t)
                                           (apply orig-fun args))))
  )

(use-package org
  :bind ("C-c n a" . org-agenda)
  :custom-face (org-agenda-date-today ((nil (:height 1.3))))
  :custom
  (org-agenda-block-separator 8411)
  (org-agenda-category-icon-alist
   `(("tech" ,(list (nerd-icons-mdicon "nf-md-laptop" :height 1.5)) nil nil :ascent center)
     ("school" ,(list (nerd-icons-mdicon "nf-md-school" :height 1.5)) nil nil :ascent center)
     ("personal" ,(list (nerd-icons-mdicon "nf-md-drama_masks" :height 1.5)) nil nil :ascent center)
     ("content" ,(list (nerd-icons-faicon "nf-fae-popcorn" :height 1.5)) nil nil :ascent center)))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-custom-commands
   '(("i" "Ideas" todo "IDEA")
     ("n" "Agenda and all TODOs"
      ((agenda "")
       (alltodo "")))))
  (org-agenda-default-appointment-duration 60)
  (org-agenda-files (list (expand-file-name "agenda/agenda.org" org-roam-directory)
                          (expand-file-name "agenda/inbox.org" org-roam-directory)))
  (org-agenda-hide-tags-regexp ".*")
  (org-agenda-include-all-todo nil)
  (org-agenda-mouse-1-follows-link t)
  (org-agenda-prefix-format
   '((agenda . " %i ")
     (todo . " %i ")
     (tags . "%c %-12:c")
     (search . "%c %-12:c")))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-start-day "+0d")
  (org-agenda-use-time-grid nil)
  (org-agenda-window-setup 'current-window)
  (org-archive-location (expand-file-name "agenda/agenda-archive.org::" org-roam-directory))
  (org-refile-use-outline-path nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  )

(use-package org
  :hook (org-archive . org-agenda-save-buffers) ; archiving
  :config
  (defun org-agenda-save-buffers ()
    "Saves opened agenda files."
    (interactive)
    (save-some-buffers t #'org-agenda-file-p))

  ;; automatically save agenda files after some commands
  (dolist (func '(org-agenda-todo
                  org-agenda-schedule
                  org-refile
                  org-agenda-do-date-later
                  org-agenda-do-date-earlier))
    (advice-add func :after
                (lambda (&rest _)
                  (when (called-interactively-p 'any)
                    (org-agenda-save-buffers)))))
  )

(use-package org
  :custom
  (org-export-allow-bind-keywords t)
  (org-export-backends '(ascii html icalendar latex odt md))
  (org-export-preserve-breaks t)
  (org-export-with-date nil)
  (org-export-with-smart-quotes t)
  (org-export-with-toc nil)
  (org-html-validation-link nil)
  ;; html5
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  )

(use-package org
  :hook (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'custom/org-resize-latex-overlays nil t)))
  :custom
  (org-preview-latex-default-process 'dvisvgm)
  (org-preview-latex-image-directory (expand-file-name "org/lateximg/" custom/user-share-emacs-directory))
  (org-latex-to-html-convert-command "latexmlc \\='literal:%i\\=' --profile=math --preload=siunitx.sty 2>/dev/null")
  :config
  (defun custom/org-resize-latex-overlays ()
    "Rescales all latex preview fragments correctly with the text size
as you zoom text. It's fast, since no image regeneration is
required."
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
                           :scale (expt text-scale-mode-step
                                        text-scale-mode-amount))))
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil)
  )

(use-package org
  :config
  ;; meow custom state (inspired by https://aatmunbaxi.netlify.app/comp/meow_state_org_speed/)
  (setq meow-org-motion-keymap (make-keymap))
  (meow-define-state org-motion
    "Org-mode structural motion"
    :lighter "[O]"
    :keymap meow-org-motion-keymap)

  (meow-define-keys 'org-motion
    '("<escape>" . meow-normal-mode)
    '("SPC" . meow-keypad)
    '("i" . meow-insert-mode)
    '("g" . meow-normal-mode)
    '("u" .  meow-undo)
    ;; Moving between headlines
    '("k" .  org-previous-visible-heading)
    '("j" .  org-next-visible-heading)
    '("<up>" .  org-previous-visible-heading)
    '("<down>" .  org-next-visible-heading)
    ;; Moving between headings at the same level
    '("p" .  org-backward-heading-same-level)
    '("n" .  org-forward-heading-same-level)
    '("<left>" .  org-backward-heading-same-level)
    '("<right>" .  org-forward-heading-same-level)
    ;; Moving subtrees themselves
    '("K" .  org-move-subtree-up)
    '("J" .  org-move-subtree-down)
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

  ;; unfolding every header when using `meow-visit'
  (advice-add 'meow-visit :before
              (lambda (&rest _)
                (if (eq major-mode 'org-mode)
                    (unless (eq org-cycle-global-status 'all)
                      (org-fold-show-all)))))
  )

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("html" . "src html"))
  ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
  ;; Otherwise, org-tempo is broken when you try to <s TAB...
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  )

(use-package org-appear
  :after org
  :hook ((org-mode . org-appear-mode)
         (org-appear-mode . org-appear-meow-setup))
  :custom
  (org-appear-trigger 'manual)
  (org-appear-autolinks t)
  :config
  (defun org-appear-meow-setup ()
    (add-hook 'meow-insert-enter-hook #'org-appear-manual-start nil t)
    (add-hook 'meow-insert-exit-hook #'org-appear-manual-stop nil t))
  )

(use-package org-auto-tangle
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
                         "#+title: ${title}\n#+filetags: %^g\n#+date: %U\n")
      :unnarrowed t)
     ("g" "video game" plain "%?"
      :target (file+head "games/${slug}.org"
                         "#+title: ${title}\n#+filetags: %^g\n#+date: %U\n#+TODO: DROPPED(d) ENDLESS(e) UNFINISHED(u) UNPLAYED(U) TODO(t) | BEATEN(b) COMPLETED(c) MASTERED(m)\n* Status\n| Region | Rating | Ownership | Achievements |\n* Notes")

      :unnarrowed t)
     ("b" "book" plain "%?"
      :target (file+head "books/${slug}.org"
                         "#+title: ${title}\n#+filetags: :books:\n#+date: %U\n#+todo: DROPPED(d) UNFINISHED(u) UNREAD(U) TODO(t) | READ(r)\n* Status\n* Notes")
      :unnarrowed t)
     ("a" "animanga" plain "%?"
      :target (file+head "animan/${slug}.org"
                         "#+title: ${title}\n#+filetags: :animan:\n#+date: %U\n#+TODO: DROPPED(d) UNFINISHED(u) TODO(t) | COMPLETED(c)\n* Anime :anime:\n* Manga :manga:")
      :unnarrowed t)
     ))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :dailie:\n"))))
  :bind (("C-c n A a" . org-roam-alias-add)
         ("C-c n A r" . org-roam-alias-remove)
         ("C-c n d c" . org-roam-dailies-capture-today)
         ("C-c n d f" . org-roam-dailies-find-date)
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
         ("C-c n T"   . org-roam-tag-remove))
  :config
  (org-roam-setup)
  (require 'org-roam-export)
  ;; if the file is dailie then increase buffer's size automatically
  ;; (require 'org-roam-dailies)
  ;; (add-hook 'org-roam-dailies-find-file-hook (lambda () (text-scale-set 3)))
  ;; (add-hook 'find-file-hook (lambda () (if (org-roam-dailies--daily-note-p) (text-scale-set 3))))
  (defun custom/org-roam-notes-dired ()
    "Opens org-roam-directory in `dired'."
    (interactive)
    (dired org-roam-directory))
  (defun custom/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all headlines in the current file."
    (interactive)
    (org-map-entries 'org-id-get-create))
  )

(use-package consult-org-roam
  :bind ("C-c n g" . consult-org-roam-search)
  :custom (consult-org-roam-grep-func #'consult-ripgrep))

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t))

(use-package toc-org
  :hook (org-mode . #'toc-org-enable)
  :custom
  (toc-org-max-depth org-indent--deepest-level)
  (toc-org-enable-links-opening t))

(use-package smartparens
  :hook (prog-mode) ;; add `smartparens-mode' to these hooks
  :config (require 'smartparens-config)) ;; load default config

(unless (custom/termux-p)

(use-package compile
  :init (setq-default compile-command nil)
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  :config
  (defun ad-compile-comint (orig-fun &rest args)
    "Sets compilation arguments to run in `comint-mode'."
    (unless (nth 1 args)
      ;; If the COMINT argument (second argument) is nil, set it to t.
      (setq args (cons (nth 0 args) (cons t (nthcdr 2 args)))))
    (apply orig-fun args))
  (advice-add 'compilation-start :around #'ad-compile-comint)
  )

(use-package lua-mode)
(use-package nix-mode)

(use-package sh-script ;; sh-script is the package that declares redirecting shell mode to treesitter mode
  :hook ((bash-ts-mode fish-mode sh-mode) . custom/sh-set-compile-command)
  :preface
  (defun custom/sh-set-compile-command ()
    "The curent buffer gets `compile-command' changed to the following:
- Current file gets an executable permission by using shell chmod, not Emacs `chmod'
- The current file gets executed"
    (if buffer-file-name
        (setq-local compile-command (concat "chmod +x " (shell-quote-argument (buffer-file-name)) " && " (shell-quote-argument (buffer-file-name))))))
  :custom (sh-basic-offset 2)
  )

(use-package cc-mode
  :hook ((c++-mode .  custom/c++-set-compile-command)
         (c++-ts-mode . (lambda () (run-hooks 'c++-mode-hook))))
  :preface
  (defun custom/c++-set-compile-command ()
    "The curent buffer gets `compile-command' changed to the following:
- The current file gets compiled using g++
- The compiled file gets executed"
    (if buffer-file-name
        (setq-local compile-command (concat "g++ " (shell-quote-argument (buffer-file-name)) " && ./a.out"))))
  :config
  ;; this is for indenting
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label 0)
  (c-set-offset 'substatement-open 0)
  )

(defalias 'elisp-mode 'emacs-lisp-mode)

(use-package bug-hunter)

(with-eval-after-load 'elisp-mode
  (defun elisp-version-update ()
    "Update version line to today date."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward ";; Version: ")
      (delete-region (point) (line-end-position))
      (insert (format-time-string "%Y%m%d"))))
  (keymap-set emacs-lisp-mode-map "C-c C-u" #'elisp-version-update))

(use-package python
  :hook (python-base-mode . (lambda () (if buffer-file-name (setq-local compile-command (concat "python " (shell-quote-argument (buffer-file-name)))))))
  )

(use-package impatient-mode
  :hook (impatient-mode . custom/impatient-open)
  :preface
  (defun custom/impatient-open ()
    "Opens/closes impatient-mode website.
Depending on `impatient-mode''s (variable) state,
httpd gets started/stopped and the impatient website gets opened
using `browse-url'."
    (if impatient-mode
        (if (httpd-running-p)
            (browse-url (concat "http://localhost:" (number-to-string httpd-port) "/imp"))
          (progn
            (httpd-start)
            (browse-url (concat "http://localhost:" (number-to-string httpd-port) "/imp"))))
      (httpd-stop))))

(use-package sgml-mode ;; `html-mode' is defined in sgml-mode package
  :hook ((html-mode . (lambda () (smartparens-mode 0)
                        (setq-local electric-pair-inhibit-predicate
                                    `(lambda (c)
                                       (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
         ;; `sgml-mode' is not derived from `prog-mode', so I add its hook manually
         (sgml-mode . (lambda () (run-hooks 'prog-mode-hook))))
  ;; :custom (css-indent-offset 2)
  :bind (:map html-mode-map
              (">" . html-close-tag))
  :config
  (defun html-close-tag (point)
    "Inserts >, closes tag, and moves to the inserted >.
It doesn't close empty tags."
    (interactive "d")
    (insert ?>)
    (let ((current-point (point))
          ;; `sgml-close-tag' does some indenting
          ;; so I disable indenting
          (indent-line-function 'ignore)
          (indent-region-function 'ignore)
          (tag (save-excursion
                 (search-backward "<")
                 (forward-char)
                 (current-word))))
      (unless (member tag html-empty-tag-list)
        (sgml-close-tag))
      (goto-char current-point)))

  (defvar html-empty-tag-list
    '("area" "base" "br" "col" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr")
    "List of empty HTML tags.")
  )

(use-package css-mode
  :custom (css-indent-offset 2))

(use-package js
  :custom (js-indent-level 2))

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
        (json "https://github.com/tree-sitter/tree-sitter-json")
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
   (sh-mode . bash-ts-mode)
   (js-json-mode . json-ts-mode)))

(use-package autoinsert
  :hook (prog-mode . auto-insert-mode)
  :custom
  (auto-insert-directory (expand-file-name "templates/" user-emacs-directory))
  (auto-insert-query nil)
  :config
  (add-to-list 'auto-insert-alist '(bash-ts-mode nil "#!/usr/bin/env bash\n\n"))
  (add-to-list 'auto-insert-alist '(sh-mode nil "#!/usr/bin/env bash\n\n"))
  (add-to-list 'auto-insert-alist '(fish-mode nil "#!/usr/bin/env fish\n\n"))
  (add-to-list 'auto-insert-alist '(python-ts-mode nil "#!/usr/bin/env python\n\n"))
  (add-to-list 'auto-insert-alist '(c++-ts-mode . "cpp.cpp"))
  (add-to-list 'auto-insert-alist '(c++-mode . "cpp.cpp"))
  (add-to-list 'auto-insert-alist '(perl-mode nil "#!/usr/bin/env perl\n\n"))
)

)

(keymap-global-set "C-c s t" 'term)
(keymap-global-set "C-c s s" 'shell)
(setq explicit-shell-file-name "/bin/bash"
      async-shell-command-buffer 'new-buffer)

(use-package fish-mode
  :mode ("\\.fish\\'")
  :custom (fish-indent-offset 2))

(use-package eshell
  ;; :hook
  ;; (eshell-mode . (lambda () (setq mode-line-format nil)))
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
  (eshell-banner-message "")
  :config
  ;; (keymap-set eshell-mode-map "C-d" #'eshell-life-is-too-much)
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert)))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eat
  :hook (eshell-load . eat-eshell-mode))

(use-package vterm
  :unless (custom/termux-p)
  :hook ((vterm-mode . (lambda () (setq mode-line-format nil)))
         (meow-normal-mode . (lambda ()
                               (if (string-equal major-mode "vterm-mode")
                                   (unless vterm-copy-mode
                                     (vterm-copy-mode 1)))))
         (meow-insert-mode . (lambda ()
                               (if (string-equal major-mode "vterm-mode")
                                   (if vterm-copy-mode
                                       (vterm-copy-mode 0))))))
  :bind (("C-c s v" . vterm))
  :custom
  (vterm-max-scrollback 5000)
  (vterm-always-compile-module t)
  :config
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  )

(use-package sudo-edit
  :bind ("C-x C-S-f" . sudo-edit-find-file))

(use-package reverso
  :bind
  ("C-c r" . reverso)
  :preface
  (defun custom/org-reverso-grammar-subtree ()
    "Narrows to the current subtree and uses `reverso-grammar-buffer'."
    (interactive)
    (org-narrow-to-subtree)
    (org-fold-show-all)
    (reverso-grammar-buffer)
    )
  :config (add-to-list 'meow-mode-state-list '(reverso-result-mode . normal)))

(use-package writeroom-mode
  :unless (custom/termux-p))

(defun custom/switch-to-buffer-other-window-for-alist (window)
  "Kind of `switch-to-buffer-other-window' but can be used in `display-buffer-alist' with body-function parameter."
  (select-window window))

(setq display-buffer-alist
      '(
        ;; ("^\\*helpful"
        ;;  (display-buffer--maybe-at-bottom)
        ;;  (window-height . 12)
        ;;  (dedicated . t))
        ;; ("\\*Help\\*"
        ;;  (display-buffer--maybe-at-bottom)
        ;;  (window-height . 12)
        ;;  ;; (dedicated . t)
        ;;  (body-function . custom/switch-to-buffer-other-window-for-alist))

        ("^CAPTURE"
         (display-buffer--maybe-at-bottom)
         (window-height . 12))
        (" \\*Agenda Commands\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 12)
         (window-parameters . ((mode-line-format . none))))
        ("\\*Org Select\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 12))
        ("\\*Org Links\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 1)
         (window-parameters . ((mode-line-format . none))))
        ("\\*Org todo\\*"
         (display-buffer--maybe-at-bottom)
         (window-parameters . ((mode-line-format . none))))
        ("\\*Org Babel Results\\*"
         (display-buffer--maybe-at-bottom))
        ("\\*org-roam\\*"
         (display-buffer-in-direction)
         (direction . right)
         (window-width . 0.33)
         (window-height . fit-window-to-buffer))

        ("\\*compilation\\*"
         (display-buffer--maybe-at-bottom)
         ;; (display-buffer-below-selected)
         (window-height . 12)
         (dedicated . t)
         ;; (body-function . custom/switch-to-buffer-other-window-for-alist)
         )
        ("\\*Compile-log\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 12)
         (body-function . custom/switch-to-buffer-other-window-for-alist))

        ("\\*which-key\\*"
         (window-parameters . ((mode-line-format . none))))

        ("\\*Messages\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ("\\*Backtrace\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ("\\*Warnings\\*"
         (display-buffer--maybe-at-bottom)
         (window-height . 12)
         (dedicated . t)
         (body-function . custom/switch-to-buffer-other-window-for-alist))
        ;; ("\\*Async Shell Command\\*"
        ;;  (display-buffer--maybe-at-bottom)
        ;;  (window-height . 12)
        ;;  (dedicated . t)
        ;;  (body-function . custom/switch-to-buffer-other-window-for-alist))
        )

      switch-to-buffer-obey-display-actions t ; `switch-to-buffer' will respect `display-buffer-alist'
      switch-to-buffer-in-dedicated-window t) ; `switch-to-buffer' will work on dedicated window

(defun window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'window-popup-frame)
    (delete-frame)))

(defmacro window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `window-popup-frame' parameter."
  `(defun ,(intern (format "window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `window-popup-frame' parameter.
Also see `window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((window-popup-frame . t)
                                (name . "window-popup-frame")))))
       (select-frame frame)
       (switch-to-buffer " window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(define-generic-mode
    'm3u-mode                      ;; name of the mode to create
  '("#")                         ;; comments start with '#'
  nil                            ;; keywords (none in this case)
  '(("^#EXTINF" . 'font-lock-keyword-face) ;; highlight #EXTINF as keyword
    ("\\.[A-Za-z0-9_]*$" . 'font-lock-string-face)) ;; highlight file extensions as strings
  '("\\.m3u\\'")                 ;; files for which to activate this mode
  nil                            ;; other functions to call
  "A mode for M3U playlist files") ;; doc string for this mode

(unless (custom/termux-p)
  (use-package mb-transient
    :init (require 'mb-transient)
    :load-path "~/dev/emacs-mb-transient/"
    :hook (mb-transient-exit . window-delete-popup-frame)
    :config
    (window-define-with-popup-frame mb-transient)
    (advice-add 'window-popup-mb-transient :after
                (lambda () (modify-frame-parameters nil `((width . 54) (height . ,(+ 27 vertico-count))))))
    )

  (use-package mb-search
    :init (require 'mb-search)
    :load-path "~/dev/emacs-mb-search/"
    :config
    (with-eval-after-load 'vertico
      (dolist (func '(mb-search-annotation
                      mb-search-area
                      mb-search-artist
                      mb-search-cdstub
                      mb-search-event
                      mb-search-instrument
                      mb-search-label
                      mb-search-place
                      mb-search-recording
                      mb-search-release
                      mb-search-release-group
                      mb-search-series
                      mb-search-tag
                      mb-search-url
                      mb-search-work))
        (add-to-list 'vertico-multiform-commands
                     `(,func (vertico-sort-function . nil))))
      )
    )
  )
