#!/bin/bash

filename=$(date +"%Y-%m-%d_%H-%M-%S".png)

CHOICE=$(echo "Full Screen
Select Area" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -i -l 2 -b -p "Screenshot Type:")

if [ "$CHOICE" = "Full Screen" ]; then
  sleep 0.4s && grimshot --notify save screen ~/Pictures/$filename

elif [ "$CHOICE" = "Select Area" ]; then
  sleep 0.4s && grimshot --notify save area ~/Pictures/$filename

else
  exit
fi
