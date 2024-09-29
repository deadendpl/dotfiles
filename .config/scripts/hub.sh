#!/usr/bin/env bash

terminal=foot

dir="${XDG_CONFIG_HOME}/scripts"

cd "$dir"

declare -a scripts=(
  "Changing root shell"
  "NVIM config as root"
  "(sway) Toggle warm colors"
  # "(sway) Screen orientation"
  "(sway) New wallpaper"
  # "(sway) Notification center"
  "(sway) Waybar re/start"
  "Bookmarks"
  "Books"
  "Power menu"
  "Sxhkd help"
  "Touchpad on Xorg"
  "Web search"
  "Emacs server re/start"
  "HP printer toggle"
)

choice=$(printf '%s\n' "${scripts[@]}" | sort | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l ${#scripts[@]} -i -b -p "Scripts" )

case $choice in
  # "(sway) Screen orientation")
  #   hypr/screen-orientation.sh
  #   ;;
  # "(sway) Notification center")
  #   swaync-client -t
  #   ;;
  "(sway) New wallpaper")
    ~/.local/bin/pyrice
    ;;
  "(sway) Toggle warm colors")
    hypr/gammastep.sh
    ;;
  "(sway) Waybar re/start")
    hypr/waybar-start.sh
    ;;
  "NVIM config as root")
    cd tweaks && $terminal -e ./nvim-as-root.sh
    ;;
  "Bookmarks")
    ./bookmarks.sh
    ;;
  "Books")
    ./books.sh
    ;;
  "Power menu")
    ./power.sh
    ;;
  "Sxhkd help" )
    bspwm/sxhkd-help.sh
    ;;
  "Web search")
    ./web-search.sh
    ;;
  "Changing root shell")
    cd tweaks && $terminal -e ./the-same-shell-as-root.sh
    ;;
  "Touchpad on Xorg")
    cd tweaks && $terminal -e ./touchpad-on-xorg.sh
    ;;
  "Emacs server re/start")
    ./emacs.sh
    ;;
  "HP printer toggle")
    $terminal -e hp-printer-toggle
    ;;
  *)
    exit 0
    ;;
esac
