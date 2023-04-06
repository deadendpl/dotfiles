#!/bin/sh

file="$HOME/Documents/bookmarks"

launcher="rofi -dmenu"

QUERY=$(cat $file | $launcher -p "Bookmarks")

if [ -n "$QUERY" ]; then
  xdg-open "${QUERY}" 2> /dev/null
fi
