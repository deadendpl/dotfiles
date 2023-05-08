#!/bin/bash

file_path="$HOME/.config/scripts/hypr/theme.sh"

old_line=$(grep "THEME=" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='THEME="Dracula"'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
