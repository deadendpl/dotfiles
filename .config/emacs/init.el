(defvar on-termux-p (and (getenv "TERMUX_VERSION"))
  "Checks if Emacs is running inside of Termux.")

(unless on-termux-p
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1)))         ; Disable visible scrollbar
(unless on-termux-p
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1)))           ; Disable the toolbar
(unless on-termux-p
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

(require 'xdg)

(defvar user-share-emacs-directory
  (expand-file-name "emacs" (xdg-data-home))
  "Directory to redirect cache/dump files.
Elisp packages cache folders/files normally clutter
`user-emacs-directory'. The same goes for some default files like
bookmarks file. In order to prevent that this variable exists.
Most of the stuff will get redirected here.")

(defun expand-file-name-user-share (file)
  "Expand a FILE to `user-share-emacs-directory'."
  (expand-file-name file user-share-emacs-directory))

(setq-default
 bookmark-default-file
 (expand-file-name-user-share "bookmarks")
 auto-save-list-file-prefix
 (expand-file-name-user-share "auto-save-list/.saves-")
 ;; custom settings that emacs autosets
 custom-file
 (expand-file-name-user-share "custom.el")
 ;; moving backup files to trash directory
 backup-directory-alist '((".*" . "~/.local/share/Trash/files"))
 tramp-persistency-file-name
 (expand-file-name-user-share "tramp")
 save-place-file
 (expand-file-name-user-share "places")
 ;; cache from urls (eww)
 url-configuration-directory
 (expand-file-name-user-share "url")
 multisession-directory
 (expand-file-name-user-share "multisession")
 request-storage-directory
 (expand-file-name-user-share "request")
 ielm-history-file-name
 (expand-file-name-user-share "ielm-history.eld"))

(when (and (featurep 'native-compile)
           package-native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name-user-share "eln-cache/")))
    (startup-redirect-eln-cache path))
  (setq native-comp-async-report-warnings-errors nil)
  ;; Silence compiler warnings as they can be disruptive
  ;; (setq-default native-comp-async-report-warnings-errors nil)
  )

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
              electric-pair-skip-self nil
              history-length t
              kill-do-not-save-duplicates t
              scroll-error-top-bottom t
              enable-local-variables :all
              save-interprogram-paste-before-kill t
              shell-command-prompt-show-cwd t
              fill-column 72)

;; showing init time in scratch buffer
(if on-termux-p
    (add-hook
     'after-init-hook
     (lambda ()
       (setq initial-scratch-message
             (concat "Initialization time: " (emacs-init-time)))))
  (setq initial-scratch-message nil))

;; this opens links in android's default apps in termux
(if on-termux-p
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
  (find-file (expand-file-name "config.org" user-emacs-directory)))

;; make utf-8 the coding system
(set-language-environment "UTF-8")

(defun make-directory-maybe (filename &optional wildcards)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))
(advice-add 'find-file :before #'make-directory-maybe)
(advice-add 'find-file-other-window :before #'make-directory-maybe)
(advice-add 'find-file-other-tab :before #'make-directory-maybe)

;; cleaning whistespace when saving file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; `conf-mode' is not derived from `prog-mode', so I add its hook manually
(add-hook 'conf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

;; removing warning when using some commands
(setq disabled-command-function nil)

(defun launch-test-emacs ()
  "Launches Emacs that only loads test init file."
  (interactive)
  (start-process "emacs-test" nil "emacs" "-Q"
                 "-l" "~/.config/emacs/test-init.el"))

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

(defun execute-extended-command-other-tab (prefixarg &optional command-name typed)
  "Execute `execute-extended-command' in a new tab."
  (interactive
   (let ((execute-extended-command--last-typed nil))
     (list current-prefix-arg
           (read-extended-command))))
  (display-buffer-in-new-tab (current-buffer) nil)
  (with-suppressed-warnings ((interactive-only execute-extended-command))
    (execute-extended-command prefixarg command-name typed)))

(keymap-global-set "C-x 4 x" #'execute-extended-command-other-window)
(keymap-global-set "C-x t x" #'execute-extended-command-other-tab)

(defun desktop-and-tabs-clear (arg)
  "Clear desktop, close all tabs."
  (interactive "P")
  (if arg
      (emacs-lock-mode 'kill))
  (desktop-clear)
  (let ((inhibit-message t))
    (tab-close-other)
    (if arg
        (setq emacs-lock-mode nil)
      (enlight-open))))

(keymap-global-set "C-x M-k" 'desktop-and-tabs-clear)

(defun indent-dwim ()
  "Indent whole buffer or region."
  (interactive)
  (if (use-region-p)
      (call-interactively 'indent-region)
    (indent-region (point-min) (point-max))))

(keymap-global-set "<remap> <indent-region>" 'indent-dwim)

(with-eval-after-load 'hl-line
  (define-advice face-at-point (:around (orig-fun &rest args))
    "Disable `hl-line-mode' temporarily if it's non-nil."
    (if hl-line-mode
        (progn
          (hl-line-mode -1)
          (prog1 (apply orig-fun args)
            (hl-line-mode 1)))
      (apply orig-fun args))))

(global-visual-wrap-prefix-mode t)

(defun scratch-buffer-other-window ()
  (interactive)
  (switch-to-buffer-other-window (get-scratch-buffer-create)))

(keymap-global-set "C-x 4 B" 'scratch-buffer-other-window)

(add-hook 'after-init-hook #'editorconfig-mode)

(defun download-file (url)
  "Download a file from an URL."
  (interactive "sURL: ")
  (let ((filename (file-name-nondirectory url))
        (destination (read-file-name "Destination: ")))
    (if (f-directory-p destination)
        (url-copy-file url (concat destination filename))
      (url-copy-file url destination))))

(use-package use-package
  ;; :init (setq use-package-enable-imenu-support t)
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)
  (use-package-always-defer t)) ; packages by default will be lazy loaded, like they will have defer: t

(use-package package
  :custom
  (package-user-dir (expand-file-name-user-share "packages/"))
  (package-gnupghome-dir (expand-file-name-user-share "gpg"))
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

(use-package gcmh
  :demand
  :config
  (add-hook 'after-init-hook #'gcmh-mode 100))

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
    (add-to-list 'meow-mode-state-list '(helpful-mode . normal)))

  (meow-setup)
  (meow-global-mode 1))

(use-package meow
  :config
  (defun kill-ring-save-visual-line ()
    "Save the region from point to the end of visual line."
    (interactive)
    (let ((end-point (save-excursion
                       (end-of-visual-line)
                       (point))))
      (save-excursion
        (kill-ring-save (point) end-point))))
  (add-to-list 'meow-selection-command-fallback
               '(meow-save . kill-ring-save-visual-line)))

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
    (advice-add command :after #'custom/pulse-line)))

;; Make ESC quit prompts immediately
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-c f c" 'custom/find-config-file)
(keymap-global-set "C-c f ." 'find-file-at-point)
(keymap-global-set "C-x K" 'kill-current-buffer)
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
(keymap-global-set "C-x 4 k" 'other-window-prefix)
(keymap-global-set "C-x B" 'scratch-buffer)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package abbrev
  :ensure nil
  :hook (text-mode . abbrev-mode) ;; `text-mode' is a parent of `org-mode'
  :bind ("C-x \"" . unexpand-abbrev)
  :config
  (if on-termux-p
      (setq abbrev-file-name
            "~/storage/shared/Sync/backup/abbrev_defs.el")
    (setq abbrev-file-name "~/Sync/backup/abbrev_defs.el")))

(use-package recentf
  :hook ((after-init . recentf-mode)
         (kill-emacs . #'recentf-save-list))
  :bind (("C-c f r" . recentf-open))
  :custom
  (recentf-save-file (expand-file-name-user-share "recentf")) ; location of the file
  (recentf-max-saved-items nil)) ; infinite amount of entries in recentf file

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package eww
  :custom (eww-auto-rename-buffer 'title))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative))

(use-package project
  :custom
  (project-list-file
   (expand-file-name-user-share "projects"))
  (project-switch-use-entire-map t))

(use-package tab-bar
  :init
  (tab-bar-mode 1)
  ;; (advice-add #'tab-new
  ;;             :after
  ;;             (lambda (&rest _) (when (yes-or-no-p "Rename tab? ")
  ;;                                 (call-interactively #'tab-rename))))
  :custom-face
  (tab-bar-tab ((nil (:inherit 'highlight :background unspecified :foreground unspecified))))
  :bind ("C-x t k" . other-tab-prefix)
  :custom
  (tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
  (tab-bar-close-button-show nil)      ;; hide tab close / X button
  (tab-bar-new-button-show nil)        ;; hide tab new / + button
  (tab-bar-tab-hints t)                ;; show tab numbers
  (tab-bar-auto-width-max nil))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom (ibuffer-default-sorting-mode 'filename/process))

(use-package ibuffer-project
  :hook (ibuffer-mode . (lambda ()
                          (setq ibuffer-filter-groups
                                (ibuffer-project-generate-filter-groups))))
  :config
  (setq ibuffer-project-root-functions
        (remove '(identity . "Directory") ibuffer-project-root-functions)))

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
        ("Instead of holding h/l, use letter finding keybindings")
        ("Use t in embark to open directory in vterm")
        ("Use M-y to paste from kill-ring with completion")))))))

(use-package ligature
  :unless on-termux-p
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("--" "---" "==" "===" "!="
     "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***"
     ";;" "!!" "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>"
     "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##"
     "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^="
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>"
     "<!--" "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<=="
     "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<"
     ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~"
     "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>"
     "<|" "||>" "<||" "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::"
     ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/=="
     "@_" "__" "???" "<:<" ";;;")))

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

(use-package nerd-icons
  :config
  (add-to-list 'nerd-icons-mode-icon-alist
               '(lisp-data-mode nerd-icons-sucicon
                                "nf-custom-scheme"
                                :face nerd-icons-orange))
  (add-to-list 'nerd-icons-mode-icon-alist
               '(conf-space-mode nerd-icons-codicon "nf-cod-settings"
                                 :face nerd-icons-lyellow))

  (add-to-list 'nerd-icons-extension-icon-alist
               '("rasi" nerd-icons-codicon "nf-cod-settings"
                 :face nerd-icons-dorange))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("cfg" nerd-icons-codicon "nf-cod-settings"
                 :face nerd-icons-dorange))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("qml" nerd-icons-devicon "nf-dev-qt"
                 :face nerd-icons-yellow))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("asc" nerd-icons-octicon "nf-oct-key"
                 :face nerd-icons-lblue))
  ;; (add-to-list 'nerd-icons-regexp-icon-alist
  ;;              '(".*" nerd-icons-octicon "nf-oct-file_binary"
  ;;                :face nerd-icons-dsilver) t)
  (add-to-list 'nerd-icons-regexp-icon-alist
               '("config$" nerd-icons-codicon "nf-cod-settings"
                 :face nerd-icons-dorange))
  (add-to-list 'nerd-icons-regexp-icon-alist
               '("rc$" nerd-icons-codicon "nf-cod-settings"
                 :face nerd-icons-dorange)))

;; (use-package nerd-icons-dired
;;   :hook (dired-mode . nerd-icons-dired-mode)
;;   :config
;;   (advice-add #'wdired-change-to-wdired-mode :before
;;               (lambda ()
;;                 (if nerd-icons-dired-mode
;;                     (nerd-icons-dired-mode -1))))
;;   (dolist (func '(wdired-finish-edit
;;                   wdired-exit
;;                   wdired-abort-changes))
;;     (advice-add func :after
;;                 (lambda ()
;;                   (unless nerd-icons-dired-mode
;;                     (nerd-icons-dired-mode 1))))))

(use-package nerd-icons-multimodal
  :vc (:url "https://github.com/abougouffa/nerd-icons-multimodal")
  :init
  (global-nerd-icons-multimodal-mode 1)
  (advice-add #'wdired-change-to-wdired-mode :before
              (lambda ()
                (if nerd-icons-multimodal-mode
                    (nerd-icons-multimodal-mode -1))
                (revert-buffer)))
  (dolist (func '(wdired-finish-edit
                  wdired-exit
                  wdired-abort-changes))
    (advice-add func :after
                (lambda ()
                  (unless nerd-icons-multimodal-mode
                    (nerd-icons-multimodal-mode 1))
                  (revert-buffer)))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . #'nerd-icons-completion-marginalia-setup))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-battery t)
  (doom-modeline-buffer-encoding 'nondefault))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom (rainbow-delimiters-max-face-count 5))

(use-package colorful-mode
  :hook (after-init . global-colorful-mode)
  :custom (global-colorful-modes
           '(prog-mode conf-mode help-mode (not special-mode)))
  ;; the default box around the colors causes line to be slightly
  ;; misaligned
  :custom-face (colorful-base ((nil (:box nil)))))

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

(if on-termux-p
    (load-theme 'doom-dracula t) ;; if on termux, use some doom theme
  (use-package ewal-doom-themes
    :demand
    :config
    (defun ewal-dark-background-p ()
      "Return non-nil if background color is dark."
      (color-dark-p (mapcar (lambda (x)
                              (/ x 255.0))
                            (color-hex-to-rgb
                             (ewal-load-color 'background)))))

    (defun ewal-setup (theme)
      "Set some faces if THEME is ewal theme."
      (when (eq theme 'ewal-doom-one)
        (let ((color (ewal--get-base-color 'green)))
          (set-face-attribute 'line-number nil
                              :foreground color
                              :inherit 'default)
          (eval
           `(with-eval-after-load 'org
              (set-face-attribute 'org-scheduled-today nil
                                  :foreground ,color)))
          (eval `(with-eval-after-load 'completion-preview
                   (set-face-attribute 'completion-preview-exact nil
                                       :underline ,color))))
        (with-eval-after-load 'hl-line
          (if (ewal-dark-background-p)
              (set-face-attribute 'hl-line nil :background "gray5")
            (set-face-attribute 'hl-line nil :background 'unspecified
                                :inherit 'highlight)))

        (with-eval-after-load 'eww
          (set-face-attribute
           'eww-form-text nil :box
           `(:line-width 1 :color ,(ewal-get-color 'foreground))))))

    (add-hook 'enable-theme-functions 'ewal-setup)

    (defun color-hex-to-rgb (hex)
      "Return list of red, green and blue colors for the hex color
string specified by HEX."
      (list (string-to-number (substring hex 1 3) 16)
            (string-to-number (substring hex 3 5) 16)
            (string-to-number (substring hex 5 7) 16)))

    (if (ewal-dark-background-p)
        (load-theme 'ewal-doom-one t)
      (progn
        (setq ewal-doom-one-brighter-comments t
              ewal-doom-one-comment-bg nil
              ewal-dark-palette-p nil)
        (load-theme 'ewal-doom-one t)))))

(add-to-list 'default-frame-alist '(alpha-background . 95))

(use-package corfu
  :init (global-corfu-mode t)
  :hook (;; (meow-insert-exit . custom/corfu-cleanup)
         ;; ((prog-mode ielm-mode) . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode)
         ((prog-mode ielm-mode) . (lambda () (setq-local corfu-auto t))))
  :custom-face
  (corfu-current ((nil (:inherit 'highlight
                                 :background unspecified
                                 :foreground unspecified))))
  :custom
  ;; (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-popupinfo-delay nil)
  (corfu-quit-no-match t)
  (global-corfu-minibuffer nil)
  (tab-always-indent 'complete)
  ;; :preface
  ;; it doesn't exit when using meow, the fix was inspired by
  ;; https://gitlab.com/daniel.arnqvist/emacs-config/-/blob/master/init.el?ref_type=heads#L147
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
  :hook (corfu-mode . nerd-icons-corfu-setup)
  :preface
  (defun nerd-icons-corfu-setup ()
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
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

(use-package completion-preview
  :hook (after-init . global-completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("<tab>" . completion-preview-insert))
  :config
  (add-to-list 'global-completion-preview-modes
               '(not prog-mode conf-mode)))

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-exit))
  :custom
  (enable-recursive-minibuffers t)
  ;; `recentf-open' will not have sorted entries
  (vertico-multiform-commands
   '((recentf-open (vertico-sort-function . nil))))
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
  (savehist-file (expand-file-name-user-share "history"))
  (savehist-additional-variables '(comint-input-ring)))

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
  (([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap project-find-file] . consult-project-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ("M-P" . consult-history)
   ([remap comint-history-isearch-backward-regexp] . consult-history)
   ([remap previous-matching-history-element] . consult-history)
   ([remap eshell-previous-matching-input] . consult-history)
   ([remap yank-pop] . consult-yank-pop))
  :custom
  (consult-async-min-input 0)
  :config
  ;; no live preview as loading org mode takes few seconds
  (consult-customize consult-buffer consult-project-buffer
                     consult-buffer-other-tab consult-buffer-other-window
                     consult-buffer-other-frame
                     :preview-key nil)
  ;; adding project source
  ;; (push 'consult--source-project-recent-file consult-buffer-sources)
  (push 'consult--source-project-buffer consult-buffer-sources)
  ;; (meow-normal-define-key '("P" . consult-yank-from-kill-ring))
  (setq consult--source-project-root
         `( :name     "Project Root"
            :narrow   ?r
            :category file
            :face     consult-file
            :history  file-name-history
            :action   ,(lambda (root)
                         (let ((default-directory root))
                           (project-find-file)))
            :items    ,#'consult--project-known-roots)))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)
              ("F" . dired-do-du))
  :custom
  (insert-directory-program "ls")
  (dired-listing-switches "-lvXAh --group-directories-first")
  (dired-switches-in-mode-line 0)
  (dired-kill-when-opening-new-dired-buffer t)
  (image-dired-dir (expand-file-name-user-share "image-dired"))
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-vc-rename-file t)
  (dired-guess-shell-alist-user
   '(("\\..*$" "xdg-open")))
  (dired-dwim-target t)
  :config
  (defun dired-do-du (files)
    "Return file size of current or marked FILES file using du."
    (interactive (list (dired-get-marked-files t)))
    (if (length> files 1)
        (shell-command (format "du -c -h -s -L %s"
                               (mapconcat
                                #'shell-quote-argument files " ")))
      (shell-command (format "du -h -s -L %s"
                             (shell-quote-argument (car files)))))
    ;; `message' gets rebound so it doesn't send anything to
    ;; messages buffer, otherwise the output of du doesn't show up
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (apply #'format args))))
      (dired-post-do-command))))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  (add-to-list 'diredfl-compressed-extensions ".chd"))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key) ; it doesn't work with meow
  ("C-h C-." . helpful-at-point-better)
  ("C-h '" . describe-face)
  :custom (helpful-max-buffers nil)
  :config
  (defun helpful-at-point-better ()
    "Improved version of `helpful-at-point'.
Handles symbols that start or end with a single quote (') correctly."
    (interactive)
    (if-let ((sym (thing-at-point 'symbol t)))
        (let ((sym (cond
                    ((char-equal ?' (aref sym 0)) ; Starts with '
                     (substring sym 1)) ; Remove leading '
                    ((char-equal ?' (aref sym (1- (length sym)))) ; Ends with '
                     (substring sym 0 -1)) ; Remove trailing '
                    ;; is in org mode and is surrounded by =
                    ((and (or (eq major-mode 'org-mode)
                              (eq major-mode 'org-agenda-mode))
                          (and (char-equal ?= (aref sym 0))
                               (char-equal ?= (aref sym (1- (length sym))))))
                     (substring sym 1 -1))
                    (t sym)))) ; No changes needed
          (helpful-symbol (intern sym)))
      (message "No symbol found at point!"))))

(use-package helpful
  :init
  (advice-add #'describe-key :override #'meow-helpful-key)
  (defun meow-helpful-key (&rest args)
    (funcall #'helpful-key (cdaar args))))

(use-package which-key
  :unless on-termux-p
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
  :unless on-termux-p
  :hook (elfeed-mode . hl-line-mode)
  :custom
  ;; cache? directory
  (elfeed-db-directory
   (expand-file-name-user-share "elfeed"))
  (elfeed-feeds '("https://sachachua.com/blog/feed/"
                  "https://planet.emacslife.com/atom.xml"))
  (elfeed-search-filter "@6-months-ago")
  :bind (:map elfeed-search-mode-map
              ("f" . elfeed-search-show-entry)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  ;; (add-to-list 'embark-default-action-overrides '(execute-extended-command . helpful-function))
  (with-eval-after-load 'meow
    (meow-define-keys 'keypad
      '("C-." . embark-act)))

  (defun delete-file-or-directory (path)
    "Delete file or directory at PATH."
    (interactive "f")
    (when (yes-or-no-p (format "Delete %s" path))
      (if (file-directory-p path)
          (delete-directory path t)
        (delete-file path))))

  (keymap-set embark-file-map "d" #'delete-file-or-directory)
  (keymap-set embark-file-map "i" #'embark-insert-relative-path)
  (keymap-set embark-file-map "I" #'embark-insert))

(use-package embark-consult)

(use-package magit
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-fullframe-status-topleft-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-repository-directories '(("~/.dotfiles" . 0)
                                  ("~/dev" . 1)))
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package transient
  :custom
  (transient-show-during-minibuffer-read t)
  (transient-history-file
   (expand-file-name-user-share "transient/history.el"))
  (transient-levels-file
   (expand-file-name-user-share "transient/levels.el"))
  (transient-values-file
   (expand-file-name-user-share "transient/values.el"))
  :bind ("C-c w r" . window-resize-transient)
  :config
  (transient-define-prefix window-resize-transient ()
    "Transient for resizing windows."
    [["Change window"
      ("o" "Change window" other-window :transient t)]
     ["Expand"
      ("v" "Vertically" enlarge-window :transient t)
      ("h" "Horizontally" enlarge-window-horizontally :transient t)]
     ["Shrink"
      ("C-v" "Vertically" shrink-window :transient t)
      ("C-h" "Horizontally" shrink-window-horizontally :transient t)]
     ["Quit"
      ("q" "Quit" transient-quit-one)]]))

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
  (org-babel-default-header-args '((:session . "none")
                                   (:results . "replace")
                                   (:exports . "both")
                                   (:cache . "no")
                                   (:noweb . "no")
                                   (:hlines . "no")
                                   (:tangle . "no")))
  (org-blank-before-new-entry nil) ;; no blank lines when doing M-return
  (org-capture-templates
   '(("t" "Todo" entry (file "agenda/inbox.org")
      "* TODO %?\nSCHEDULED: %^t")))
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-display-remote-inline-images 'download)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file
   (expand-file-name-user-share "org/.org-id-locations"))
  (org-image-actual-width '(300 600))
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-insert-heading-respect-content t)
  (org-link-file-path-type 'relative)
  (org-list-allow-alphabetical t)
  (org-log-done 'time)
  ;; time tamps from headers and etc. get put into :LOGBOOK: drawer
  (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-src-preserve-indentation t)
  (org-startup-folded t)
  (org-startup-indented t) ; use `org-indent-mode' at startup
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
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
  (advice-add 'org-edit-src-save :before
              (lambda ()
                (delete-trailing-whitespace (point-min) (point-max)))))

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
      (org-return))))

(use-package org
  :config
  (define-advice org-meta-return
      (:around (orig-fun &rest args))
    "Depending on the context, it insert things differently."
    (cond
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading t))
     ((ignore-errors (org-entry-is-todo-p))
      (if (org-at-item-p)
          (apply orig-fun args)
        (org-insert-todo-heading t)))
     ;; we're in a list with ::
     ((ignore-errors (let* ((itemp (org-in-item-p))
                            (struct (save-excursion (goto-char itemp)
                                                    (org-list-struct)))
                            (prevs (org-list-prevs-alist struct))
                            (desc (eq (org-list-get-list-type
                                       itemp struct prevs)
		                              'descriptive)))
                       desc))
      ;; if current item has ::, use default function,
      ;; otherwise insert an item without ::
      (if-let* ((itemp (org-in-item-p))
                (struct (save-excursion (goto-char itemp)
                                        (org-list-struct)))
                (prevs (org-list-prevs-alist struct))
                (desc (nth 5 (seq-find (lambda (item)
                                         (eq itemp (car item)))
                                       struct))))
          (apply orig-fun args)
        (let* ((itemp (org-in-item-p))
               (pos (save-excursion (1+ (progn (end-of-line) (point)))))
               (struct (save-excursion (goto-char itemp)
                                       (org-list-struct)))
               (prevs (org-list-prevs-alist struct)))
          (org-list-insert-item pos struct prevs)
          (end-of-line))))
     (t (apply orig-fun args))))
  ;; (defun org-insert-dwim (orig-fun &rest args)
;;     "Depending on the context, it insert things differently.
;; It is meant to be used with advice functions."
;;     (cond
;;      ((org-at-item-checkbox-p)
;;       (org-insert-todo-heading t))
;;      ((ignore-errors (org-entry-is-todo-p))
;;       (if (org-at-item-p)
;;           (apply orig-fun args)
;;         (org-insert-todo-heading t)))
;;      ;; we're in a list with ::
;;      ((ignore-errors (let* ((itemp (org-in-item-p))
;;                             (struct (save-excursion (goto-char itemp)
;;                                                     (org-list-struct)))
;;                             (prevs (org-list-prevs-alist struct))
;;                             (desc (eq (org-list-get-list-type
;;                                        itemp struct prevs)
;; 		                              'descriptive)))
;;                        desc))
;;       ;; if current item has ::, use default function,
;;       ;; otherwise insert an item without ::
;;       (if-let* ((itemp (org-in-item-p))
;;                 (struct (save-excursion (goto-char itemp)
;;                                         (org-list-struct)))
;;                 (prevs (org-list-prevs-alist struct))
;;                 (desc (nth 5 (seq-find (lambda (item)
;;                                          (eq itemp (car item)))
;;                                        struct))))
;;           (apply orig-fun args)
;;         (let* ((itemp (org-in-item-p))
;;                (pos (save-excursion (1+ (progn (end-of-line) (point)))))
;;                (struct (save-excursion (goto-char itemp)
;;                                        (org-list-struct)))
;;                (prevs (org-list-prevs-alist struct)))
;;           (org-list-insert-item pos struct prevs)
;;           (end-of-line))))
;;      (t (apply orig-fun args))))

  ;; (advice-add 'org-meta-return :around 'org-insert-dwim)
  )

(use-package org
  :bind ("C-c n a" . org-agenda)
  :custom-face (org-agenda-date-today ((nil (:height 1.3))))
  :custom
  (org-agenda-block-separator 8411)
  ;; (org-agenda-category-icon-alist
  ;;  `(("tech" (,(nerd-icons-mdicon "nf-md-laptop" :height 1.5))
  ;;     nil nil :ascent center)
  ;;    ("school" (,(nerd-icons-mdicon "nf-md-school" :height 1.5))
  ;;     nil nil :ascent center)
  ;;    ("personal" (,(nerd-icons-mdicon "nf-md-drama_masks" :height 1.5))
  ;;     nil nil :ascent center)
  ;;    ("content" (,(nerd-icons-faicon "nf-fae-popcorn" :height 1.5))
  ;;     nil nil :ascent center)))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-custom-commands
   '(("i" "Ideas" todo "IDEA")
     ("n" "Agenda and all TODOs"
      ((agenda "")
       (alltodo "")))))
  (org-agenda-default-appointment-duration 60)
  (org-agenda-files
   `(,(expand-file-name "agenda/agenda.org" org-roam-directory)
     ,(expand-file-name "agenda/inbox.org" org-roam-directory)))
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
  (org-archive-location
   (expand-file-name "agenda/agenda-archive.org::" org-roam-directory))
  (org-refile-use-outline-path nil)
  ;; (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  )

(use-package org
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
                  org-agenda-do-date-earlier
                  org-archive-subtree
                  org-agenda-refile
                  org-agenda-archive))
    (advice-add func :after
                (lambda (&rest _)
                  (when (called-interactively-p 'any)
                    (org-agenda-save-buffers))))))

(use-package org
  :custom
  (org-export-allow-bind-keywords t)
  (org-export-backends '(ascii html icalendar latex odt md))
  (org-export-preserve-breaks t)
  (org-export-with-date nil)
  (org-export-with-smart-quotes t)
  (org-export-with-toc nil)
  (org-export-babel-evaluate nil)
  (org-html-validation-link nil)
  ;; html5
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-html-postamble nil)
  (org-publish-timestamp-directory
   (expand-file-name-user-share "org/timestamps")))

(use-package org
  :init
  (define-minor-mode org-latex-resize-mode
    "Automatic latex resizing."
    :global nil
    (if org-latex-resize-mode
        (add-hook 'text-scale-mode-hook
                  #'custom/org-resize-latex-overlays nil t)
      (remove-hook 'text-scale-mode-hook
                   #'custom/org-resize-latex-overlays t)))
  :hook (org-mode . org-latex-resize-mode)
  :custom
  (org-preview-latex-default-process 'dvisvgm)
  (org-preview-latex-image-directory
   (expand-file-name-user-share "org/lateximg/"))
  (org-latex-to-html-convert-command
   (concat "latexmlc \\='literal:%i\\=' "
           "--profile=math --preload=siunitx.sty 2>/dev/null"))
  :config
  (defun custom/org-resize-latex-overlays ()
    "Rescales all latex preview fragments correctly with the text size
as you zoom text. It's fast, since no image regeneration is required."
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
                           :scale (expt text-scale-mode-step
                                        text-scale-mode-amount))))
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil))

(use-package org
  :bind (:map org-mode-map
              ("C-c M-m" . meow-org-motion-mode))
  :config
  ;; meow custom state (inspired by
  ;; https://aatmunbaxi.netlify.app/comp/meow_state_org_speed/)
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
  (add-hook 'org-mode-hook #'org-electric-pair-disable-arrow)
  (defun org-electric-pair-disable-arrow ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<) t
                     (,electric-pair-inhibit-predicate c)))))
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
    (add-hook 'meow-insert-exit-hook #'org-appear-manual-stop nil t)))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  (if on-termux-p
      (setq org-roam-directory "~/storage/shared/org-roam")
    (setq org-roam-directory "~/org-roam"))
  :hook (org-roam-find-file . org-roam-set-modified-date-setup)
  :custom
  (org-directory org-roam-directory)
  (org-roam-db-location (expand-file-name
                         "org-roam.db"
                         (concat org-roam-directory "/attachments")))
  (org-roam-dailies-directory "journals/")
  (org-roam-node-display-template
   (concat "${title} " (propertize "${tags}" 'face 'org-tag)))
  (org-roam-file-exclude-regexp '("attachments/"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org"
                         "%(org-roam-created-date-property)\n#+title: ${title}\n#+filetags: %^g\n")
      :unnarrowed t)
     ("g" "video game" plain "%?"
      :target (file+head "games/${slug}.org"
                         "%(org-roam-created-date-property)\n#+title: ${title}\n#+filetags: :games:%^g\n#+TODO: DROPPED(d) ENDLESS(e) UNFINISHED(u) UNPLAYED(U) TODO(t) | BEATEN(b) COMPLETED(c) MASTERED(m)\n* Status\n| Region | Rating | Ownership | Achievements |\n* Notes")

      :unnarrowed t)
     ("b" "book" plain "%?"
      :target (file+head "books/${slug}.org"
                         "%(org-roam-created-date-property)\n#+title: ${title}\n#+filetags: :books:\n#+todo: DROPPED(d) UNFINISHED(u) UNREAD(U) TODO(t) | READ(r)\n* Status\n* Notes")
      :unnarrowed t)
     ("a" "animanga" plain "%?"
      :target (file+head "animan/${slug}.org"
                         "%(org-roam-created-date-property)\n#+title: ${title}\n#+filetags: :animan:\n#+TODO: DROPPED(d) UNFINISHED(u) TODO(t) | COMPLETED(c)\n* Anime :anime:\n* Manga :manga:")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?" :target
      (file+head "%<%Y-%m-%d>.org"
                 "%(org-roam-created-date-property)\n#+title: %<%Y-%m-%d>\n#+filetags: :dailie:\n"))))
  (org-persist-directory
   (expand-file-name-user-share "org/org-persist/"))
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
         ("C-c n p"   . org-roam-increment-progress-property)
         ("C-c n r"   . org-roam-ref-add)
         ("C-c n R"   . org-roam-ref-remove)
         ("C-c n t"   . org-roam-tag-add)
         ("C-c n T"   . org-roam-tag-remove)
         :map org-mode-map ("C-c C-S-l" . org-move-file-and-insert-link))
  :preface
  (defun custom/org-roam-notes-dired ()
    "Opens org-roam-directory in `dired'."
    (interactive)
    (dired org-roam-directory))
  (defun org-roam-created-date-property ()
    "Return a property drawer with created date."
    (concat
     ":PROPERTIES:\n"
     ":CREATED_AT: "
     (format-time-string "[%Y-%m-%d %a]")
     "\n:END:"))
  :config
  (org-roam-setup)
  (require 'org-roam-export)
  ;; if the file is dailie then increase buffer's size automatically
  ;; (require 'org-roam-dailies)
  ;; (add-hook 'org-roam-dailies-find-file-hook (lambda () (text-scale-set 3)))
  ;; (add-hook 'find-file-hook (lambda () (if (org-roam-dailies--daily-note-p) (text-scale-set 3))))
  (defun custom/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all headlines in the current file."
    (interactive)
    (org-map-entries 'org-id-get-create))

  (defun org-move-file-and-insert-link (file)
    "Move FILE to attachment directory and link to it."
    (interactive "fFile: ")
    (let ((new-file (concat (expand-file-name "attachments/"
                                              org-roam-directory)
                            (file-name-nondirectory file))))
      (rename-file file new-file 1)
      (org-insert-link nil (format "file:%s" new-file))))

  (defun org-roam-set-modified-date-property ()
    "Update the updated at property in org roam file."
    (save-excursion
      (goto-char (point-min))
      (org-set-property "MODIFIED_AT"
                        (format-time-string "[%Y-%m-%d %a]"))))

  (defun org-roam-set-modified-date-setup ()
    (add-hook 'before-save-hook
              'org-roam-set-modified-date-property nil t))

  (defun org-increment-progress-property ()
    "Increment value of progress property at point.
It's value needs to be number/anything.
123/124 or 45/? for example."
    (interactive)
    (let* ((orig (org-entry-get (point) "progress"))
           (split (split-string orig "/"))
           (number (string-to-number (car split)))
           (rest (cadr split)))
      (org-set-property "progress" (format "%s/%s" (1+ number) rest))))

  (defun org-roam-increment-progress-property ()
    "Increment value of progress property of chosen node."
    (interactive)
    (save-window-excursion
      (let ((node (org-roam-node-read)))
        (org-roam-node-open node)
        (save-excursion
          ;; org-roam node has property drawer at the very beginning
          ;; of a file
          (goto-char (point-min))
          (if (org-entry-get (point) "progress")
              (org-increment-progress-property)
            (consult-org-heading)
            (org-increment-progress-property))
          (save-buffer)
          (message "Updated %s progress to %s"
                   (org-roam-node-title (org-roam-node-at-point))
                   (org-entry-get (point) "progress")))))))

(use-package consult-org-roam
  :bind ("C-c n g" . consult-org-roam-search)
  :custom (consult-org-roam-grep-func #'consult-ripgrep))

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t))

(use-package org-roam
  :config
  (defun org-roam-complete-link ()
    "Create a org roam link using completion."
    (concat "roam:" (org-roam-node-title (org-roam-node-read))))

  (org-link-set-parameters "roam" :complete 'org-roam-complete-link)

  (define-advice org-insert-link
      (:after (&rest _))
    "Replace all :roam links with ID links."
    (org-roam-link-replace-all)))

(use-package toc-org
  :hook (org-mode . #'toc-org-enable)
  :custom
  (toc-org-max-depth org-indent--deepest-level)
  (toc-org-enable-links-opening t))

(use-package compile
  :init (setq-default compile-command nil)
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  :config
  (define-advice compilation-start
      (:around (orig-fun &rest args) start-in-comint-mode)
    "Sets compilation arguments to run in `comint-mode'."
    (unless (nth 1 args)
      ;; If the COMINT argument (second argument) is nil, set it to t.
      (setq args (cons (nth 0 args) (cons t (nthcdr 2 args)))))
    (apply orig-fun args)))

(use-package lua-mode)
(use-package nix-mode)

(use-package sh-script
  :hook ((bash-ts-mode fish-mode sh-mode) . custom/sh-set-compile-command)
  :preface
  (defun custom/sh-set-compile-command ()
    "The curent buffer gets `compile-command' changed to the following:
- Current file gets an executable permission by using shell chmod, not Emacs `chmod'
- The current file gets executed"
    (if buffer-file-name
        (setq-local compile-command
                    (shell-quote-argument (buffer-file-name)))))
  :custom (sh-basic-offset 2))

(use-package cc-mode
  :hook ((c++-mode .  custom/c++-set-compile-command)
         (c++-ts-mode . (lambda () (run-hooks 'c++-mode-hook))))
  :preface
  (defun custom/c++-set-compile-command ()
    "The curent buffer gets `compile-command' changed to the following:
- The current file gets compiled using g++
- The compiled file gets executed"
    (if buffer-file-name
        (setq-local compile-command
                    (concat "g++ "(shell-quote-argument
                                   (buffer-file-name)) " && ./a.out"))))
  :config
  ;; this is for indenting
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label 0)
  (c-set-offset 'substatement-open 0)
  (setcdr (assoc 'other c-default-style) "linux"))

(use-package subword
  :hook (after-init . global-subword-mode))

(use-package inf-lisp
  :custom (inferior-lisp-program "sbcl --noinform"))

(use-package sly
  :custom (sly-mrepl-history-file-name
           (expand-file-name-user-share "sly-mrepl-history"))
  :bind (:map sly-mode-map
              ("C-c C-e" . sly-eval-buffer)))

(defalias 'elisp-mode 'emacs-lisp-mode)

(use-package bug-hunter)

(with-eval-after-load 'elisp-mode
  (defun elisp-version-update ()
    "Update version line to today date."
    (interactive)
    (let ((date (format-time-string "%Y%m%d")))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward ";; Version: " nil t)
          (delete-region (point) (line-end-position))
          (insert date))
        (goto-char (point-min))
        (when (re-search-forward "^(def\\(var\\|const\\).*[0-9]" nil t)
          (delete-backward-char (length (thing-at-point 'symbol t)))
          (insert date)))
      (when (buffer-modified-p)
        (let ((inhibit-message t))
          (if (yes-or-no-p "Save the file?")
              (save-buffer)))
        (message (concat "Version updated to " date)))))
  (keymap-set emacs-lisp-mode-map "C-c C-u" #'elisp-version-update))

(use-package python
  :hook (python-base-mode . (lambda () (if buffer-file-name (setq-local compile-command (concat "python " (shell-quote-argument (buffer-file-name)))))))
  :custom (python-indent-offset 2))

(use-package sgml-mode ;; `html-mode' is defined in sgml-mode package
  :hook ((html-mode . (lambda ()
                        (setq-local electric-pair-inhibit-predicate
                                    `(lambda (c)
                                       (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
         ;; `sgml-mode' is not derived from `prog-mode', so I add its hook manually
         (sgml-mode . (lambda () (run-hooks 'prog-mode-hook))))
  :config
  (keymap-set sgml-mode-map "RET" #'newline-and-indent)
  (defun html-close-tag ()
    "Closes tag.
The tag is closed if the last typed character was > and if there
were no > before it.
It doesn't close empty tags."
    (when (and (eql last-command-event ?>)
               (not (eql (char-before (1- (point))) ?>)))
      (save-excursion
        (if-let (;; `sgml-close-tag' does some indenting
                 ;; so I disable indenting
                 (indent-line-function 'ignore)
                 (indent-region-function 'ignore)
                 (tag (save-excursion
                        (when (search-backward "<" nil t)
                          (forward-char)
                          ;; when we have </foo, return nil
                          (and (not (= (following-char) ?/))
                               (current-word))))))
            (unless (member tag html-empty-tag-list)
              (sgml-close-tag))))))
  (add-hook 'html-mode-hook (lambda ()
                              (add-hook 'post-self-insert-hook
                                        'html-close-tag nil t)))

  (defvar html-empty-tag-list
    '("area" "base" "br" "col" "embed" "hr" "img" "input" "keygen"
      "link" "meta" "param" "php" "source" "track" "wbr")
    "List of empty HTML tags."))

(use-package css-mode
  :custom (css-indent-offset 2))

(use-package js
  :custom (js-indent-level 2)
  :hook (js-mode . (lambda () (setq indent-tabs-mode nil))))

(unless on-termux-p
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          ;; (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          ;; (make "https://github.com/alemuller/tree-sitter-make")
          ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (php "https://github.com/tree-sitter/tree-sitter-php")))
  ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
  ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (unless (eq (car lang) 'php) ; php doesn't install
      (treesit-install-language-grammar (car lang)))))

(setq major-mode-remap-alist
      '((c-or-c++-mode . c-or-c++-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (js-json-mode . json-ts-mode)
        (js-mode . js-ts-mode))))

(use-package autoinsert
  :hook (prog-mode . auto-insert-mode)
  :custom
  (auto-insert-directory
   (expand-file-name "templates/" user-emacs-directory))
  (auto-insert-query nil)
  :config
  (add-to-list 'auto-insert-alist
               '(bash-ts-mode nil"#!/usr/bin/env bash\n\n"))
  (add-to-list 'auto-insert-alist
               '(sh-mode nil "#!/usr/bin/env bash\n\n"))
  (add-to-list 'auto-insert-alist
               '(fish-mode nil "#!/usr/bin/env fish\n\n"))
  (add-to-list 'auto-insert-alist
               '(python-ts-mode nil "#!/usr/bin/env python\n\n"))
  (add-to-list 'auto-insert-alist '(c++-ts-mode . "cpp.cpp"))
  (add-to-list 'auto-insert-alist '(c++-mode . "cpp.cpp"))
  (add-to-list 'auto-insert-alist
               '(perl-mode nil "#!/usr/bin/env perl\n\n"))
  (add-to-list 'auto-insert-alist
               '(("\\.user.js\\'" . "JavaScript userscript")
                 nil
                 "// ==UserScript==" \n
                 "// @name         " (read-string "Name: ") \n
                 "// @version      " (format-time-string "%Y.%m.%d") \n
                 "// @description  " (read-string "Description: ") \n
                 "// @author       "
                 (concat user-full-name
                         (if (string= user-full-name "")
                             user-mail-address
                           (concat " (" user-mail-address ")"))) \n
                 "// @match        " (read-string "URL: " "https://") \n
                 "// ==/UserScript==" \n \n
                 "(function() {" \n
                 "'use strict';" \n -
                 "})();" '(beginning-of-line) '(delete-char 2))))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook
             'after-save-hook
             'executable-make-buffer-file-executable-if-script-p
             nil t)))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(keymap-global-set "C-c s t" 'term)
(keymap-global-set "C-c s s" 'shell)
(setq explicit-shell-file-name "/bin/bash"
      async-shell-command-buffer 'new-buffer)

(use-package fish-mode
  :custom (fish-indent-offset 2))

(use-package eshell
  ;; :hook
  ;; (eshell-mode . (lambda () (setq mode-line-format nil)))
  :bind (("C-c s e" . eshell)
         :map eshell-mode-map
         ("C-d" . eshell-quit-or-delete-char))
  :custom
  (eshell-directory-name
   (expand-file-name "eshell" user-emacs-directory))
  ;; your profile for eshell; like a bashrc for eshell
  (eshell-rc-script
   (expand-file-name "profile" eshell-directory-name))
  ;; sets an aliases file for the eshell
  (eshell-aliases-file
   (expand-file-name "aliases" eshell-directory-name))
  (eshell-history-file-name
   (expand-file-name-user-share "eshell-history"))
  (eshell-last-dir-ring-file-name
   (expand-file-name-user-share "eshell-lastdir"))
  (eshell-history-size 5000)
  (eshell-buffer-maximum-lines 5000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input nil)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-banner-message "")
  :config
  ;; (keymap-set eshell-mode-map "C-d" #'eshell-life-is-too-much)
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (defun eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
          (eshell-life-is-too-much))
      (delete-forward-char arg))))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eat
  :hook (eshell-load . eat-eshell-mode))

(use-package vterm
  :unless on-termux-p
  :init
  ;; embark setup
  (with-eval-after-load 'embark
    (defun vterm-dir (directory)
      "Spawn vterm in specified DIRECTORY."
      (interactive "D")
      (let ((default-directory directory))
        (vterm)))
    (keymap-set embark-file-map "t" 'vterm-dir))
  :hook (vterm-mode . (lambda () (setq mode-line-format nil)))
  (vterm-mode . vterm-meow-setup)
  :bind (("C-c s v" . vterm))
  :custom
  (vterm-max-scrollback 5000)
  (vterm-always-compile-module t)
  :config
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (defun vterm-meow-setup ()
    (add-hook 'meow-normal-mode-hook
              (lambda ()
                (if (string-equal major-mode "vterm-mode")
                    (unless vterm-copy-mode
                      (vterm-copy-mode 1))))
              nil t)
    (add-hook 'meow-insert-mode-hook
              (lambda ()
                (if (string-equal major-mode "vterm-mode")
                    (if vterm-copy-mode
                        (vterm-copy-mode 0))))
              nil t)))

(use-package sudo-edit
  :bind ("C-x C-S-f" . sudo-edit-find-file))

(use-package reverso
  :bind
  ("C-c r" . reverso)
  :preface
  (defun custom/org-reverso-grammar-subtree ()
    "Check grammar in a narrowed subtree."
    (interactive)
    (org-narrow-to-subtree)
    (org-fold-show-all)
    (reverso-grammar-buffer))
  :config
  (add-to-list 'meow-mode-state-list '(reverso-result-mode . normal)))

(use-package writeroom-mode
  :unless on-termux-p)

(defun custom/switch-to-buffer-other-window-for-alist (window)
  "Kind of `switch-to-buffer-other-window' but can be used in
`display-buffer-alist' with body-function parameter."
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

(define-generic-mode m3u-mode      ; name of the mode to create
  '("#")                           ; comments start with '#'
  nil                              ; keywords (none in this case)
  ;; highlight #EXTINF as keyword, it doesn't work, because lines
  ;; starting with '#' are treated as comments
  '(("^#EXTINF" . 'font-lock-keyword-face)
    ;; highlight file extensions as strings
    ("\\.[A-Za-z0-9_]*$" . 'font-lock-string-face))
  '("\\.m3u\\'" "\\.m3u8\\'")      ; files for which to activate this mode
  '((lambda () (setq mode-name "M3U"))) ; other functions to call
  "A mode for M3U playlist files") ; doc string for this mode

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist
               '(m3u-mode nerd-icons-mdicon
                          "nf-md-playlist_music_outline"
                          :face nerd-icons-dred)))

(unless on-termux-p
  (use-package mb-transient
    :init
    (window-define-with-popup-frame mb-transient)
    (advice-add 'window-popup-mb-transient :after
                (lambda () (modify-frame-parameters nil '((width . 54)))
                  (set-window-parameter nil 'mode-line-format 'none)))
    :load-path "~/dev/emacs-mb-transient/"
    :hook (mb-transient-exit . window-delete-popup-frame)
    :commands (mb-transient)
    :config
    (add-to-list 'vertico-multiform-commands
                 '(mb-transient--search (vertico-sort-function . nil))))

  (use-package mb-search
    :load-path "~/dev/emacs-mb-search/"
    :commands (mb-search-annotation
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
               mb-search-work)
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
                     `(,func (vertico-sort-function . nil)))))))

(defun anilist-get-weekly-progress ()
  "Create a buffer with last week activity on AniList."
  (interactive)
  (let ((query
         (format "{\"query\": \"query ExampleQuery { Page(page: 1, perPage: 1000) { activities: activities(userId: 6071947, createdAt_greater: %s) { ... on ListActivity { createdAt media { format id title { romaji english native } } progress status type } } } }\", \"variables\": {}}"
                 (let ((one-week-ago (time-subtract (current-time)
                                                    (days-to-time 7))))
                   (format-time-string "%s" one-week-ago)))))
    (let ((buffer (generate-new-buffer "anilist-weekly-progress")))
      (with-current-buffer buffer
        (lisp-data-mode)
        (call-process
         "curl" nil t nil "-s"
         "-X" "POST"
         "--header" "Content-Type: application/json"
         "--data" query
         "--url" "https://graphql.anilist.co")
        (goto-char (point-min))
        (let ((out (json-read)))
          ;; converting unix timestamps to normal dates
          (mapcar
           (lambda (item)
             (setcdr (car item)
                     (format-time-string "%Y-%m-%d" (cdr (car item)))))
           (append (cdar (cdadar out)) nil))
          (goto-char (point-max))
          (delete-backward-char (buffer-size))
          (save-excursion
            (pp out buffer))))
      (switch-to-buffer-other-window buffer))))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
  :custom-face (aw-leading-char-face ((nil (:inherit highlight :foreground nil)))))
