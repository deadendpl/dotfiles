#!/usr/bin/env bash

filename=$(date +"%Y-%m-%d_%H-%M-%S".png)

CHOICE=$(printf "Full Screen\nSelect Area\nColor Picker" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -i -l 3 -p "Screenshot Type:")

mkdir -p ~/Pictures

if [ "$CHOICE" = "Full Screen" ]; then
  sleep 0.4s && grimshot --notify save screen ~/Pictures/$filename

elif [ "$CHOICE" = "Select Area" ]; then
  grimshot --notify save area ~/Pictures/$filename

elif [ "$CHOICE" = "Color Picker" ]; then
  hyprpicker -a

else
  exit
fi
