# This dmenu script lets you use rofi to search web
#!/bin/bash

URL='https://search.brave.com/search?q='      # variable for search engine, you can change it if you want
QUERY=$(echo '' | rofi -dmenu -p "Search" -fn "-xos4-terminus-medium-r-*-*-14-*" -b)
if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
fi