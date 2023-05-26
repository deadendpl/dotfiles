#!/bin/bash

file_path="$HOME/.config/polybar/config.ini"

old_line=$(grep "include-file = ~/.config/polybar/colors" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="include-file = ~/.config/polybar/colors/catppuccin-latte.ini"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
