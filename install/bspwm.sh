#!/bin/bash

cd ../dependencies/bspwm/
./arch.sh
./aur.sh

cd ../../home/.config

# Check if there are any directories present in ~/.config
if [[ $(ls -d ~/.config/*/ 2>/dev/null) ]]; then
  # Prompt the user before overwriting folders in ~/.config
  read -rp "WARNING: ~/config directory is not empty. Proceed with overwriting existing files? [y/N] " confirm
  if [[ ! "$confirm" =~ [Yy] ]]; then
    echo "Operation aborted by user." >&2
    exit 1
  fi
fi

# Move all directories from the source directory to the destination directory
mv -f * ~/.config/
