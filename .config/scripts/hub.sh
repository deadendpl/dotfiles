#!/usr/bin/env bash

if [[ $XDG_CURRENT_DESKTOP == "Hyprland" ]]; then
  terminal=foot
else
  terminal=alacritty
fi

dir="$HOME/.config/scripts"

cd "$dir"

declare -a scripts=( "Changing root shell" "NVIM config as root" "(hypr) Toggle warm colors" "(hypr) Screen orientation" "(hypr) New wallpaper" "(hypr) Notification center" "(hypr) Waybar re/start" "Bookmarks" "Books" "Power menu" "Sxhkd help" "Touchpad on Xorg" "Web search" "Emacs server re/start" )

choice=$(printf '%s\n' "${scripts[@]}" | sort | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#scripts[@]} -i -b -p "Scripts" )

if [[ $choice == "(hypr) Screen orientation" ]]; then
  hypr/screen-orientation.sh

elif [[ $choice == "(hypr) Notification center" ]]; then
  swaync-client -t

elif [[ $choice == "(hypr) New wallpaper" ]]; then
  ~/.local/bin/pyrice

elif [[ $choice == "(hypr) Toggle warm colors" ]]; then
  hypr/gammastep.sh

elif [[ $choice == "(hypr) Waybar re/start" ]]; then
  hypr/waybar-start.sh

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

elif [[ $choice == "Emacs server re/start" ]]; then
  ./emacs.sh

else
  exit
fi
