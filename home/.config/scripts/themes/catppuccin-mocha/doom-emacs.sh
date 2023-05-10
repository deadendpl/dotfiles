#!/bin/bash

file_path="$HOME/.config/doom/config.el"

old_line=$(grep "setq doom-theme" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="(setq doom-theme 'catppuccin)"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

~/.config/emacs/bin/doom sync
