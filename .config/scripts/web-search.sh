#!/usr/bin/env bash

declare -a search_engines=(
  "Anna's Archive"
  "AUR (Arch User Repository)"
  "Arch Wiki"
  "Arch Packages"
  "Brave Search"
  "Discogs"
  "Ecosia"
  "GameFAQs"
  "Github"
  "MusicBrainz"
  "Nitter (Twitter/X)"
  "Nix Packages"
  "NixOS Wiki"
  "YouTube (Invidious)"
  "YouTube (Piped)"
  "Phind"
  "SearXNG"
  "VGMdb"
  "Whoogle"
) # add or remove search engines here

selected_engine=$(printf '%s\n' "${search_engines[@]}" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l ${#search_engines[@]} -i -p "Search engine:")

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
  "YouTube (Invidious)")
    URL="https://farside.link/invidious/search?q="
    ;;
  "YouTube (Piped)")
    URL="https://farside.link/piped/results?search_query="
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
  "Anna's Archive")
    URL="https://annas-archive.org/search?q="
    ;;
  "Ecosia")
    URL="https://www.ecosia.org/search?method=index&q="
    ;;
  "Discogs")
    URL="https://www.discogs.com/search?q="
    ;;
  "MusicBrainz")
    # ${XDG_CONFIG_HOME}/scripts/mb-search.sh
    emacsclient -e '(mb-transient-frame)'
    exit 0
    ;;
  "VGMdb")
    URL="https://vgmdb.net/search?q="
    QUERY=$(echo '' | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l 0 -p "Search $selected_engine:") || exit
    xdg-open "${URL}${QUERY}&type=" 2> /dev/null
    exit 0
    ;;
  *)
    exit 0
    ;;
esac

QUERY=$(echo '' | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l 0 -p "Search $selected_engine:")

if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
fi
