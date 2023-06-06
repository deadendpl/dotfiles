#!/bin/bash

file_path="$HOME/.config/hypr/hyprland.conf"

# Get the line containing "@import"
old_line=$(grep "terminal=" "$file_path")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='$terminal=foot -c ~/.config/foot/dracula.ini'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

file_path="$HOME/.config/hypr/README.org"

# Get the line containing "@import"
old_line=$(grep "terminal=" "$file_path")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='$terminal=foot -c ~/.config/foot/dracula.ini'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

unset $file_path

unset $file_path

file_path="$HOME/.config/scripts/hypr/wrappedhl"

# Get the line containing "@import"
old_line=$(grep "terminal=" "$file_path")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='export terminal="foot -c $HOME/.config/foot/dracula.ini"'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

