#!/bin/bash

directory=~/.config/scripts/themes/catppuccin-mocha
exclude_script=./ALL.sh

cd "$directory"

for script in *; do
  if [ "$script" != "$exclude_script" ] && [ -x "$script" ] && [ -f "$script" ]; then
    if [ "$script" != "${0##*/}" ]; then
      ./"$script"
    fi
  fi
done

echo "I don't know yet how to change gtk3 theme so you need to do that yourself with lxappearance on bspwm or gnome-tweaks on hyprland."

echo "It's recommended to log out or reboot your machine."

read -p "Press Enter to exit"
