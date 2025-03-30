#!/usr/bin/env bash

declare -a search_engines=(
  "Anna's Archive"
  "AUR (Arch User Repository)"
  "Arch Wiki"
  "Arch Packages"
  "Brave"
  "Cover Music Hoarders"
  "Discogs"
  "Ecosia"
  "GameFAQs"
  "GitHub"
  "MusicBrainz (MB)"
  "Nix Packages"
  "NixOS Wiki"
  "YouTube (Invidious)"
  "YouTube (Piped)"
  "SearXNG"
  "VGMdb"
  "Wikipedia"
)

selected_engine=$(printf '%s\n' "${search_engines[@]}" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l ${#search_engines[@]} -i -p "Search engine:")

case $selected_engine in
  "Cover Music Hoarders")
    URL="https://covers.musichoarders.xyz/?album="
    ;;
  "SearXNG")
    URL="https://farside.link/searxng/search?q="
    ;;
  "Brave")
    URL="https://search.brave.com/search?q="
    # QUERY=$(echo '' | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l 0 -p "Search $selected_engine:") || exit
    # xdg-open "${URL}${QUERY}&source=desktop" 2> /dev/null
    # exit 0
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
  "GitHub")
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
  "GameFAQs")
    URL="https://gamefaqs.gamespot.com/search?game="
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
  "MusicBrainz (MB)")
    # ${XDG_CONFIG_HOME}/scripts/mb-search.sh
    emacsclient -e '(window-popup-mb-transient)'
    exit 0
    ;;
  "VGMdb")
    URL="https://vgmdb.net/search?q="
    QUERY=$(echo '' | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l 0 -p "Search $selected_engine:") || exit
    xdg-open "${URL}${QUERY}&type=" 2> /dev/null
    exit 0
    ;;
  "Wikipedia")
    URL="https://wikipedia.org/w/index.php?&search="
    ;;
  *)
    exit 0
    ;;
esac

QUERY=$(echo '' | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l 0 -p "Search $selected_engine:")

if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
fi
