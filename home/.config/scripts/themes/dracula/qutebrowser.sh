#!/bin/bash

file_path="$HOME/.config/qutebrowser/config.py"

old_line=$(grep "import" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="import dracula.draw"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi


line_to_delete="catppuccin.setup(c, 'mocha', True)"

# Use sed to delete the line
sed -i "/$line_to_delete/d" "$file_path"

echo "Line deleted successfully."

# Uncomment the specific lines
pattern="dracula.draw.blood(c, {"

# Uncomment the specific lines
sed -i '/dracula\.draw\.blood(c, {/,/})/ s/^# //' "$file_path"

echo "Lines uncommented successfully."
