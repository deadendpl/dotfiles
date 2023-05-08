#!/bin/bash

theme=$(printf "Dracula
Catppuccin Mocha" | rofi -config ~/.config/rofi/config-bare.rasi -l 2 -dmenu -p "Themes")

if [ "$theme" == "Dracula" ]; then
   $terminal -e ~/.config/scripts/themes/dracula/ALL.sh
elif [ "$theme" == "Catppuccin Mocha" ]; then
   $terminal -e ~/.config/scripts/themes/catppuccin/ALL.sh
else
   exit
fi
