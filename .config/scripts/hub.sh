#!/usr/bin/env bash

if [[ $XDG_CURRENT_DESKTOP == "Hyprland" ]]; then
  terminal=foot
else
  terminal=alacritty
fi

dir="$HOME/.config/scripts"

cd "$dir"

declare -a scripts=( "Changing root shell" "NVIM config as root" "(hypr) Screen orientation" "(hypr) New wallpaper" "(hypr) Waybar reload" "Bookmarks" "Books" "Power menu" "Sxhkd help" "Touchpad on Xorg" "Web search" )

choice=$(printf '%s\n' "${scripts[@]}" | sort | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#scripts[@]} -i -b -p "Scripts" )

if [[ $choice == "(hypr) Screen orientation" ]]; then
  ~/.config/scripts/hypr/screen-orientation.sh

elif [[ $choice == "(hypr) New wallpaper" ]]; then
  ~/.local/bin/pyrice

elif [[ $choice == "(hypr) Waybar reload" ]]; then
  ~/.config/scripts/hypr/waybar-start.sh

elif [[ $choice == "NVIM config as root" ]]; then
  cd tweaks && $terminal -e ./nvim-as-root.sh

elif [[ $choice == "Bookmarks" ]]; then
  ./bookmarks.sh

elif [[ $choice == "Books" ]]; then
  ./books.sh

elif [[ $choice == "Power menu" ]]; then
  ./power.sh

elif [[ $choice == "Sxhkd help"  ]]; then
  bspwm/sxhkd-help.sh

elif [[ $choice == "Web search" ]]; then
  ./web-search.sh

elif [[ $choice == "Changing root shell" ]]; then
  cd tweaks && $terminal -e ./the-same-shell-as-root.sh

elif [[ $choice == "Touchpad on Xorg" ]]; then
  cd tweaks && $terminal -e ./touchpad-on-xorg.sh

else
  exit
fi
