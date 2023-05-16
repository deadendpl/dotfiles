#!/bin/bash

declare -a themes=( "Dracula" "Catppuccin Latte" "Catppuccin Mocha" )

theme=$(printf '%s\n' "${themes[@]}" | sort | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#themes[@]} -i -b -p "Themes:")

if [ "$theme" == "Dracula" ]; then
   $terminal -e ~/.config/scripts/themes/dracula/ALL.sh
elif [ "$theme" == "Catppuccin Mocha" ]; then
   $terminal -e ~/.config/scripts/themes/catppuccin-mocha/ALL.sh
elif [ "$theme" == "Catppuccin Latte" ]; then
   $terminal -e ~/.config/scripts/themes/catppuccin-latte/ALL.sh
else
   exit
fi
