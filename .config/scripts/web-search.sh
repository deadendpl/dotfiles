#!/usr/bin/env bash

declare -a search_engines=( "Melpa" "NixOS Wiki" "Nitter (Twitter/X)" "Nix Packages" "Whoogle" "GameFAQs" "Gutenberg" "Bitsearch (Torrents)" "Invidious (YouTube)" "Github" "Arch Wiki" "Arch Packages" "AUR (Arch User Repository)" "Brave Search" "Phind" "SearXNG" ) # add or remove search engines here

selected_engine=$(printf '%s\n' "${search_engines[@]}" | sort | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l ${#search_engines[@]} -i -p "Search engine:")

case $selected_engine in
    "Phind")
        URL="https://www.phind.com/search?q="
        ;;
    "SearXNG")
        URL="https://farside.link/searxng/search?q="
        ;;
    "Brave Search")
        URL="https://search.brave.com/search?q="
        ;;
    "AUR (Arch User Repository)")
        URL="https://aur.archlinux.org/packages?O=0&K="
        ;;
    "Arch Wiki")
        URL="https://wiki.archlinux.org/index.php?search="
        ;;
    "Arch Packages")
        URL="https://archlinux.org/packages/?q="
        ;;
    "Github")
        URL="https://github.com/search?q="
        ;;
    "Invidious (YouTube)")
        URL="https://farside.link/invidious/search?q="
        ;;
    "Bitsearch (Torrents)")
        URL="https://www.bitsearch.to/search?q="
        ;;
    "Gutenberg")
        URL="https://www.gutenberg.org/ebooks/search/?query="
        ;;
    "Nix Packages")
        URL="https://search.nixos.org/packages?query="
        ;;
    "NixOS Wiki")
        URL="https://nixos.wiki/index.php?search="
        ;;
    "Whoogle")
        URL="https://farside.link/whoogle/search?q="
        ;;
    "GameFAQs")
        URL="https://gamefaqs.gamespot.com/search?game="
        ;;
    "Nitter (Twitter/X)")
        URL="https://farside.link/nitter/search?&q="
        ;;
    "Melpa")
        URL="https://melpa.org/#/?q="
        ;;
  *)
        exit 0
        ;;
esac

QUERY=$(echo '' | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l 0 -p "Search $selected_engine:")

if [ -n "$QUERY" ]; then
    xdg-open "${URL}${QUERY}" 2> /dev/null
fi
