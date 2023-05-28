#!/bin/bash

file_path="$HOME/.config/scripts/hypr/wall.sh"

old_line=$(grep "walls_dir=" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using awk
    new_line='walls_dir="$HOME/Pictures/cat-walls"'
    awk -v old_line="$old_line" -v new_line="$new_line" '{ if ($0 == old_line) { print new_line } else { print $0 } }' "$file_path" > temp_file && mv temp_file "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

chmod +x $file_path

file_path="$HOME/.config/scripts/bspwm/wall.sh"

old_line=$(grep "walls_dir=" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using awk
    new_line='walls_dir="$HOME/Pictures/cat-walls"'
    awk -v old_line="$old_line" -v new_line="$new_line" '{ if ($0 == old_line) { print new_line } else { print $0 } }' "$file_path" > temp_file && mv temp_file "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

chmod +x $file_path
