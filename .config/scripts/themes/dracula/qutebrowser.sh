#!/usr/bin/env bash

file_path="$HOME/.config/qutebrowser/config.py"

old_line=$(grep "config.source" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="config.source('dracula.py')"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
