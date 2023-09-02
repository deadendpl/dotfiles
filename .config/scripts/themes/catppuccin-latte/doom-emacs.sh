#!/usr/bin/env bash

file_path="$HOME/.config/doom/config.el"

old_theme_line=$(grep "setq doom-theme" "$file_path")
old_flavour_line=$(grep "setq catppuccin-flavor" "$file_path")

if [ -n "$old_theme_line" ]; then
    # Replace the theme line using sed
    new_theme_line="(setq doom-theme 'catppuccin)"
    sed -i "s|$old_theme_line|$new_theme_line|" "$file_path"
    echo "Theme line replaced successfully!"
else
    echo "Theme line not found in the file."
fi

if [ -n "$old_flavour_line" ]; then
    # Replace the flavour line using sed
    new_flavour_line="(setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha"
    sed -i "s|$old_flavour_line|$new_flavour_line|" "$file_path"
    echo "Flavour line replaced successfully!"
else
    echo "Flavour line not found in the file."
fi

~/.config/emacs/bin/doom sync
