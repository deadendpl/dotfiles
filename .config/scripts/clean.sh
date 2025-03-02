#!/usr/bin/env bash

declare -a forbidden_files=(
  "${XDG_CACHE_HOME}/wal"
  "${XDG_CACHE_HOME}/keepassxc"
  "${XDG_CACHE_HOME}/rofi3.druncache"
  "${XDG_CACHE_HOME}/rofi-entry-history.txt"
)

forbidden_file_p() {
  local path="$1"
  for foo in "${forbidden_files[@]}"; do
    if [[ "$path" == "$foo" ]]; then
      return 0  # Found, return success
    fi
  done
  return 1  # Not found, return failure
}

# Cleanup commands
yay --noconfirm -Sc && yay -Qtdq | yay --noconfirm -Rns -

echo "Deleting files in pacman cache"
find /var/cache/pacman/pkg/ -maxdepth 1 -type f -exec sudo rm {} \;

echo "Deleting files in cache"
for file in $(find "$XDG_CACHE_HOME" -maxdepth 1 | tail -n +2); do
  if forbidden_file_p "$file"; then
    sleep 0
  else
    rm -rf "$file"
  fi
done

echo "Deleting files in trash"
find "${XDG_DATA_HOME}/Trash/" -mindepth 1 -exec rm -r {} \;

echo "Deleting logs"
sudo rm -r /var/log/*
