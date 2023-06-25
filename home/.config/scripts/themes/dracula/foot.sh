#!/bin/bash

file_path="$HOME/.config/foot/foot.ini"

# Get the line containing "@import"
old_line=$(grep "include" $file_path)

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='include = ~/.config/foot/dracula.ini'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
