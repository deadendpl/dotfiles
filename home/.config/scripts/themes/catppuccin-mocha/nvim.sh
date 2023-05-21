#!/bin/bash

file_path="$HOME/.config/nvim/lua/opts.lua"

old_line=$(grep "theme = " "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="    theme = 'catppuccin-mocha'"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
