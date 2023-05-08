#!/bin/bash

file_path="$HOME/.config/wezterm/wezterm.lua"

old_line=$(grep "include-file = ~/.config/polybar/" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="include-file = ~/.config/polybar/dracula.ini"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
