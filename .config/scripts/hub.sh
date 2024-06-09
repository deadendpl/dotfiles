#!/usr/bin/env bash

if [[ $XDG_CURRENT_DESKTOP == "Hyprland" ]]; then
  terminal=foot
fi

dir="${XDG_CONFIG_HOME}/scripts"

cd "$dir"

declare -a scripts=(
  "Changing root shell"
  "NVIM config as root"
  "(hypr) Toggle warm colors"
  "(hypr) Screen orientation"
  "(hypr) New wallpaper"
  "(hypr) Notification center"
  "(hypr) Waybar re/start"
  "Bookmarks"
  "Books"
  "Power menu"
  "Sxhkd help"
  "Touchpad on Xorg"
  "Web search"
  "Emacs server re/start"
)

choice=$(printf '%s\n' "${scripts[@]}" | sort | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l ${#scripts[@]} -i -b -p "Scripts" )

case $choice in
  "(hypr) Screen orientation")
    hypr/screen-orientation.sh
    ;;
  "(hypr) Notification center")
    swaync-client -t
    ;;
  "(hypr) New wallpaper")
    ~/.local/bin/pyrice
    ;;
  "(hypr) Toggle warm colors")
    hypr/gammastep.sh
    ;;
  "(hypr) Waybar re/start")
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
  *)
    exit 0
    ;;
esac
