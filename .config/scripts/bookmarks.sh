#!/bin/sh

file="$HOME/Documents/bookmarks"

sort $file -o $file

options="Enter other URL\n$(cat $file)"

QUERY=$(echo -e "$options" | $launcher -theme ~/.config/rofi/themes/drac-list.rasi -dmenu -p Bookmarks)

if [ "$QUERY" = "Enter other URL" ]; then
  QUERY=$(echo "" | $launcher -dmenu -p "Enter URL")
fi

if [ -n "$QUERY" ]; then
  xdg-open "${QUERY}" 2> /dev/null
fi
