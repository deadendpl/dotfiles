#!/usr/bin/env bash

file_path="$HOME/.config/nvim/init.lua"

old_line=$(grep "theme = " "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="    theme = 'catppuccin-latte'"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

file_path="$HOME/.config/nvim/README.org"

old_line=$(grep "theme = " "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="    theme = 'catppuccin-latte'"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
