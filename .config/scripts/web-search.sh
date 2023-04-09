#!/bin/bash

URL='https://search.brave.com/search?q='      # variable for search engine, you can change it if you want
QUERY=$(echo '' | $launcher -dmenu -l 0 -b -p Search)
if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
fi
