#!/bin/bash

# show rofi menu to select screenshot type
CHOICE=$(echo "Full Screen
Select Area" | $launcher -theme ~/.config/rofi/themes/drac-list.rasi -dmenu -i -b -p "Screenshot Type:")

# take screenshot based on user's choice
if [ "$CHOICE" = "Full Screen" ]; then
  # take full screen screenshot using grim
  grim
else
  # take partial screenshot using slurp and grim
  grim -g "$(slurp)"
fi

# show notification after taking screenshot
notify-send "Screenshot taken"

