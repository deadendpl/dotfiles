#!/usr/bin/env bash

filename=$(date +"%Y-%m-%d_%H-%M-%S".png)

CHOICE=$(printf "Full Screen\nSelect Area\nColor Picker" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -i -l 3 -p "Screenshot Type:")

mkdir -p ~/Pictures

case $CHOICE in
  "Full Screen")
    sleep 0.4s && grimshot --notify save screen ~/Pictures/$filename
    ;;
  "Select Area")
    grimshot --notify save area ~/Pictures/$filename
    ;;
  "Color Picker")
    sleep 0.4 && hyprpicker -a
    ;;
esac
