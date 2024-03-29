#!/usr/bin/env bash

file_path="$HOME/.config/bspwm/bspwmrc"

old_line=$(grep "dunst" "$file_path")

if [ -n "$old_line" ]; then
    # Escape special characters in the old line
    escaped_old_line=$(sed 's/[\*\.&]/\\&/g' <<< "$old_line")

    # Escape special characters in the new line
    escaped_new_line=$(sed 's/[\*\.&]/\\&/g' <<< "dunst -config ~/.config/dunst/catppuccin-latte.conf &")

    # Replace the line using sed
    sed -i "s|$escaped_old_line|$escaped_new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

file_path="$HOME/.config/bspwm/README.org"

old_line=$(grep "dunst -config" "$file_path")

if [ -n "$old_line" ]; then
    # Escape special characters in the old line
    escaped_old_line=$(sed 's/[\*\.&]/\\&/g' <<< "$old_line")

    # Escape special characters in the new line
    escaped_new_line=$(sed 's/[\*\.&]/\\&/g' <<< "dunst -config ~/.config/dunst/catppuccin-latte.conf &")

    # Replace the line using sed
    sed -i "s|$escaped_old_line|$escaped_new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
