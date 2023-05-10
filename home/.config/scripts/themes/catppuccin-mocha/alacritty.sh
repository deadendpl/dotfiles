#!/bin/bash

file_path="$HOME/.config/alacritty/alacritty.yml"

old_line=$(grep "~/.config/alacritty" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="  - ~/.config/alacritty/catppuccin-mocha.yml"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
