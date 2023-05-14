#!/bin/bash

directory=~/.config/scripts/themes/dracula
exclude_script=./ALL.sh

cd "$directory"

for script in *; do
  if [ "$script" != "$exclude_script" ] && [ -x "$script" ] && [ -f "$script" ]; then
    if [ "$script" != "${0##*/}" ]; then
      ./"$script"
    fi
  fi
done

echo "It's recommended to log out or reboot your machine."

read -p "Press Enter to exit"
