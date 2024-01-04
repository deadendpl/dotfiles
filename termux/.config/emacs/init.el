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
