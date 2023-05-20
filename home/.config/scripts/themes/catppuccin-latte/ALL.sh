#!/bin/bash

directory=~/.config/scripts/themes/catppuccin-latte
exclude_script=./ALL.sh

cd "$directory"

for script in *; do
  if [ "$script" != "$exclude_script" ] && [ -x "$script" ] && [ -f "$script" ]; then
    if [ "$script" != "${0##*/}" ]; then
      ./"$script"
    fi
  fi
done

echo "For now gtk3 theme is changing only in hyprland. If you use bspwm then you need to set theme manually in lxappearance."

echo "It's recommended to log out or reboot your machine."

read -p "Press Enter to exit"
