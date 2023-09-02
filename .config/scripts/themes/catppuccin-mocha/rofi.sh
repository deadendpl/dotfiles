#!/usr/bin/env bash

file_path="$HOME/.config/rofi/config.rasi"

old_line=$(grep "@theme" "$file_path")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='@theme "~/.config/rofi/themes/catppuccin-mocha-list.rasi"'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully in $file_path!"
else
    echo "Old line not found in $file_path."
fi

unset file_path

file_path="$HOME/.config/rofi/config-bare.rasi"

old_line=$(grep "@theme" "$file_path")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='@theme "~/.config/rofi/themes/catppuccin-mocha-list.rasi"'
    sed -i "s|$old_line|$new_line|" "$file_path"
    echo "Line replaced successfully in $file_path!"
else
    echo "Old line not found in $file_path."
fi

unset file_path

file_path="$HOME/.config/scripts/power.sh"

old_line=$(grep " -theme" "$file_path")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Create a temporary file with the new line
    new_line='chosen=$(echo -e "󰍃 Logout\n Shutdown\n Reboot\n󰤄 Suspend" | rofi -config ~/.config/rofi/config-bare.rasi -theme ~/.config/rofi/themes/catppuccin-mocha-small.rasi -theme-str '\''window {width: 12%;}'\'' -dmenu -l 4 -i -p Power)'
    tmp_file=$(mktemp)
    echo "$new_line" > "$tmp_file"

    # Replace the line using the temporary file
    sed -i -e "/ -theme/ {r $tmp_file" -e "d}" "$file_path"
    rm "$tmp_file"

    echo "Line replaced successfully in $file_path!"
else
    echo "Old line not found in $file_path."
fi
