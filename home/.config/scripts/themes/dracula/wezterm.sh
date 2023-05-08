#!/bin/bash

file_path="$HOME/.config/wezterm/wezterm.lua"

old_line=$(grep "config.color_scheme" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="config.color_scheme = 'Dracula'"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
