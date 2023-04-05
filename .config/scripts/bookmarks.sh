#!/bin/sh

file="$HOME/Documents/bookmarks"

QUERY=$(cat $file | rofi -dmenu -p "Bookmarks")

if [ -n "$QUERY" ]; then
  xdg-open "${QUERY}" 2> /dev/null
fi
