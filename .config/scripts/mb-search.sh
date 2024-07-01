#!/usr/bin/env bash

mb_url="https://musicbrainz.org"
type="type="
method="method="

declare -a options=(
  "Indexed"
  "Indexed with Advanced Query Syntax"
  "Direct Database Search"
)

option=$(printf '%s\n' "${options[@]}" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l ${#options[@]} -i -p "Search method:")

case $option in
  "Indexed")
    method="${method}indexed"
    ;;
  "Indexed with Advanced Query Syntax")
    method="${method}advanced"
    ;;
  "Direct Database Search")
    method="${method}direct"
    ;;
  *)
    exit
    ;;
esac

declare -a options=(
  "Artist"
  "Documentation"
  "Recording"
  "Release"
  "Release Group"
)

option=$(printf '%s\n' "${options[@]}" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l ${#options[@]} -i -p "Option:")

case $option in
  "Artist")
    type="${type}artist"
    ;;
  "Documentation")
    type="${type}doc"
    ;;
  "Recording")
    type="${type}recording"
    ;;
  "Release")
    type="${type}release"
    ;;
  "Release Group")
    type="${type}release_group"
    ;;
  *)
    exit 0
    ;;
esac

query=$(echo '' | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l 0 -p "Query:") || exit

xdg-open "${mb_url}/search?query=${query}&${type}&${method}"
