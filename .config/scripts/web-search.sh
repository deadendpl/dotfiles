#!/bin/bash

URL='https://www.phind.com/search?q='      # variable for search engine, you can change it if you want
QUERY=$(echo '' | $launcher -theme ~/.config/rofi/themes/drac-list.rasi -dmenu -l 0 -b -p Search)
if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
fi
