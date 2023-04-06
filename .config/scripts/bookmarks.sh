#!/bin/sh

file="$HOME/Documents/bookmarks"

launcher="dmenu -l 10"

QUERY=$(cat $file | $launcher -p "Bookmarks")

if [ -n "$QUERY" ]; then
  xdg-open "${QUERY}" 2> /dev/null
fi
