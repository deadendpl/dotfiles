#!/bin/bash

file_path="$HOME/.config/qutebrowser/config.py"

# Check if "import" line exists
if grep -q "import" "$file_path"; then
    # Replace the "import" line
    sed -i "s/^import.*/import catppuccin/" "$file_path"
    echo "Import line replaced successfully!"
else
    # Add "import" line before "catppuccin.setup" line
    sed -i "/catppuccin\.setup/i import catppuccin" "$file_path"
    echo "Import line added successfully!"
fi

# Check if "catppuccin.setup" line exists
if grep -q "catppuccin\.setup" "$file_path"; then
    # Replace the line using sed
    sed -i "s/catppuccin\.setup.*/catppuccin.setup(c, 'latte', True)/" "$file_path"
    echo "Line replaced successfully!"
else
    echo "catppuccin.setup line not found in the file."
fi

# Comment the specific lines
sed -i '201s/^/# /' "$file_path"
sed -i '202s/^/# /' "$file_path"
sed -i '203s/^/# /' "$file_path"
sed -i '204s/^/# /' "$file_path"
sed -i '205s/^/# /' "$file_path"
sed -i '206s/^/# /' "$file_path"

echo "Lines commented successfully."
