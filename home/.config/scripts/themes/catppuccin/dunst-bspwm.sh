#!/bin/bash

file_path="$HOME/.config/bspwm/bspwmrc"

old_line=$(grep "dunst" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="dunst -config ~/.config/dunst/catppuccin &"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
