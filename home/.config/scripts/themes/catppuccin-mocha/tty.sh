#!/bin/bash

file_path="$HOME/.config/fish/config.fish"

old_line=$(grep "source ~" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="source ~/.config/tty-colors/fish/catppuccin-mocha"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

file_path="$HOME/.bashrc"

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="source ~/.config/tty-colors/bash/catppuccin-mocha"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi
