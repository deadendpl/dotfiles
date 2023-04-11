#!/bin/sh

file="$HOME/Documents/bookmarks"

sort $file -o $file

QUERY=$(cat $file | $launcher -theme ~/.config/rofi/themes/drac-list.rasi -dmenu -p Bookmarks)

if [ -n "$QUERY" ]; then
  xdg-open "${QUERY}" 2> /dev/null
fi
