#!/bin/bash

# Define the question and choices
QUESTION="What rice would you like to install?
Note that it will delete your .config folder. Make an backup."
CHOICES=("bspwm" "" "off" "hyprland" "" "off")

# Pipe the question and choices into whiptail
OPTIONS=$(whiptail --title "Installation" --separate-output --checklist "$QUESTION" 15 60 2 \
    "bspwm" "Install bspwm" ON \
    "hyprland" "Install hyprland" OFF \
    3>&1 1>&2 2>&3)

# If the user selects bspwm, execute the bspwm.sh script
if echo "$OPTIONS" | grep -q "bspwm"; then
  exec ./bspwm.sh
fi

# If the user selects hyprland, execute the hyprland.sh script
if echo "$OPTIONS" | grep -q "hyprland"; then
  exec ./hypr.sh
fi

# If the user cancels, do nothing
if [ -z "$OPTIONS" ]; then
  echo "Installation canceled."
fi

