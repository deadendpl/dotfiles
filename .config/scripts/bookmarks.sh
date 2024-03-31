#!/usr/bin/env bash

file="$HOME/Sync/foo/wazne/bookmarks"

if [[ -f $file ]]; then
  sort $file -o $file

  options="$(cat $file)"

  QUERY=$(echo -e "$options" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -p Bookmarks)

  if [ -n "$QUERY" ]; then
    if [[ $QUERY == http://* || $QUERY == https://* ]]; then
      xdg-open "${QUERY}" 2> /dev/null
    else
      xdg-open "https://${QUERY}" 2> /dev/null
    fi
  fi
else
  notify-send "Bookmark file was created at $file"
  touch $file
fi
