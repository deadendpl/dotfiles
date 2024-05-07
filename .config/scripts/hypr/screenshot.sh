#!/usr/bin/env bash

directory="$HOME/Pictures"
filename=$(date +"$directory/%Y-%m-%d_%H-%M-%S".png)

notification() {
  local choice=$(notify-send --action 'action1=Open screenshot' "Screenshot of screen" "$filename")

  case $choice in
    "action1")
      xdg-open $filename
      ;;
    *)
      ;;
  esac
}

choice=$(printf "Full Screen\nSelect Area\nColor Picker" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -i -l 3 -p "Screenshot Type:")

mkdir -p ~/Pictures

case $choice in
  "Full Screen")
    sleep 0.4s && grimshot save screen $filename
    notification
    ;;
  "Select Area")
    grimshot save area $filename
    notification
    ;;
  "Color Picker")
    sleep 0.4 && hyprpicker -a
    ;;
esac
