#!/bin/bash

declare -a search_engines=( "Arch Wiki" "AUR (Arch User Repository)" "You" "Brave Search" "Google" "DuckDuckGo" "Bing" "Phind" "SearXNG" ) # add or remove search engines here

selected_engine=$(printf '%s\n' "${search_engines[@]}" | sort | $launcher -theme ~/.config/rofi/themes/drac-list.rasi -dmenu -l ${#search_engines[@]} -i -b -p "Search engine:")

case $selected_engine in
    "Google")
        URL="https://www.google.com/search?q="
        ;;
    "DuckDuckGo")
        URL="https://duckduckgo.com/?q="
        ;;
    "Bing")
        URL="https://www.bing.com/search?q="
        ;;
    "Phind")
        URL="https://www.phind.com/search?q="
        ;;
    "SearXNG")
        URL="https://searx.mha.fi/search?q="
        ;;
    "Brave Search")
        URL="https://search.brave.com/search?q="
        ;;
    "You")
        URL="https://you.com/search?q="
	;;
    "AUR (Arch User Repository)")
        URL="https://aur.archlinux.org/packages?O=0&K="
	;;
    "Arch Wiki")
        URL="https://wiki.archlinux.org/index.php?search="
	;;

    *)
        exit 0
        ;;
esac

QUERY=$(echo '' | rofi -theme ~/.config/rofi/themes/drac-list.rasi -dmenu -l 0 -b -p "Search $selected_engine:")

if [ -n "$QUERY" ]; then
    xdg-open "${URL}${QUERY}" 2> /dev/null
fi
