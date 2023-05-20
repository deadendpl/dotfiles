#!/bin/bash

dir="$HOME/.config/scripts"

cd "$dir"

declare -a scripts=( "(hypr) Screen orientation" "Bookmarks" "Books" "Power menu" "Sxhkd help" "Themes" "Web search" )

choice=$(printf '%s\n' "${scripts[@]}" | sort | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#scripts[@]} -i -b -p "Scripts" )

if [[ $choice == "(hypr) Screen orientation" ]] ; then
  ~/.config/scripts/hypr/screen-orientation.sh

elif [[ $choice == "Bookmarks" ]]; then
  ./bookmarks.sh

elif [[ $choice == "Books" ]]; then
  ./books.sh

elif [[ $choice == "Power menu" ]]; then
  ./power.sh

elif [[ $choice == "Sxhkd help"  ]]; then
  ./sxhkd-help.sh

elif [[ $choice == "Themes" ]]; then
  themes/main.sh

elif [[ $choice == "Web search" ]]; then
  ./web-search.sh

else
  exit
fi
