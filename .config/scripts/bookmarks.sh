#!/bin/sh

file="$HOME/Documents/bookmarks"

sort $file -o $file

QUERY=$(cat $file | $launcher -dmenu -l 10 -p "Bookmarks")

if [ -n "$QUERY" ]; then
  xdg-open "${QUERY}" 2> /dev/null
fi
