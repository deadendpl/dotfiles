#!/bin/bash

file_path="$HOME/.config/qutebrowser/config.py"

old_line=$(grep "import" "$file_path")

if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line="import catppuccin \ncatppuccin.setup(c, 'mocha', True)"
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

# Comment the specific lines
sed -i '201s/^/# /' "$file_path"
sed -i '202s/^/# /' "$file_path"
sed -i '203s/^/# /' "$file_path"
sed -i '204s/^/# /' "$file_path"
sed -i '205s/^/# /' "$file_path"
sed -i '206s/^/# /' "$file_path"

echo "Lines commented successfully."
