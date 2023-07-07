#!/usr/bin/env bash

dir="$HOME/.config/scripts"

cd "$dir"

declare -a scripts=( "Changing root shell" "NVIM config as root" "(hypr) Screen orientation" "Bookmarks" "Books" "Power menu" "Sxhkd help" "Themes" "Web search" )

choice=$(printf '%s\n' "${scripts[@]}" | sort | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#scripts[@]} -i -b -p "Scripts" )

if [[ $choice == "(hypr) Screen orientation" ]]; then
  ~/.config/scripts/hypr/screen-orientation.sh

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

elif [[ $choice == "Themes" ]]; then
  themes/main.sh

elif [[ $choice == "Web search" ]]; then
  ./web-search.sh

elif [[ $choice == "Changing root shell" ]]; then
  cd tweaks && $terminal -e ./the-same-shell-as-root.sh

else
  exit
fi
