#!/usr/bin/env bash

# Define the configuration file path
config_file="$HOME/.config/hypr/hyprland.conf"

# Define the lines to identify
horizontal_line="monitor=,preferred,auto,auto"
vertical_line="monitor=,preferred,auto,auto,transform,1"

# Define the options for rofi
declare -a options=("Horizontal -" "Vertical |")

# Show the rofi menu and get the selected option
selected_option=$(printf '%s\n' "${options[@]}" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#options[@]} -p "Select display rotation")

# Comment out the horizontal line
sed -i "s|^$horizontal_line$|#&|" "$config_file"

# Comment out the vertical line
sed -i "s|^$vertical_line$|#&|" "$config_file"

# Modify the configuration based on the selected option
if [[ "$selected_option" == "Vertical |" ]]; then
    # Uncomment the vertical line
    sed -i "s|^#$vertical_line$|$vertical_line|" "$config_file"
    hyprctl reload
elif [[ "$selected_option" == "Horizontal -" ]]; then
    # Uncomment the horizontal line
    sed -i "s|^#$horizontal_line$|$horizontal_line|" "$config_file"
    hyprctl reload
fi
