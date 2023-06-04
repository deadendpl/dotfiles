#!/bin/bash

file_path="$HOME/.config/gtk-3.0/settings.ini"

old_line=$(grep "gtk-theme-name" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="gtk-theme-name=Catppuccin-Latte-Standard-Pink-Light"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

old_line=$(grep "gtk-icon-theme-name" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="gtk-icon-theme-name=Papirus-Light"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
