#!/bin/bash

theme=$(printf "Dracula
Catppuccin Mocha
Catppuccin Latte" | rofi -config ~/.config/rofi/config-bare.rasi -l 3 -dmenu -p "Themes")

if [ "$theme" == "Dracula" ]; then
   $terminal -e ~/.config/scripts/themes/dracula/ALL.sh
elif [ "$theme" == "Catppuccin Mocha" ]; then
   $terminal -e ~/.config/scripts/themes/catppuccin-mocha/ALL.sh
elif [ "$theme" == "Catppuccin Latte" ]; then
   $terminal -e ~/.config/scripts/themes/catppuccin-latte/ALL.sh
else
   exit
fi
