#!/bin/sh

file="$HOME/Documents/bookmarks"

sort $file -o $file

options="Enter other URL\n$(cat $file)"

QUERY=$(echo -e "$options" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -p Bookmarks)

# if you'll choose "Enter other URL" you need to put whole https link

if [ "$QUERY" = "Enter other URL" ]; then
  QUERY=$(echo "" | $launcher -dmenu -p "Enter URL")
fi

if [ -n "$QUERY" ]; then
  if [[ $QUERY == http://* || $QUERY == https://* ]]; then
    xdg-open "${QUERY}" 2> /dev/null
  else
    xdg-open "https://${QUERY}" 2> /dev/null
  fi
fi
