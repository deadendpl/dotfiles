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
sudo rm -r /var/cache/pacman/pkg/

echo "Deleting files in cache"
for file in $(find "$XDG_CACHE_HOME" -maxdepth 1 | tail -n +2); do
  if forbidden_file_p "$file"; then
    sleep 0
  else
    rm -rf "$file"
  fi
done

echo "Deleting files in trash"
rm -r "${XDG_DATA_HOME}/Trash/"
mkdir "${XDG_DATA_HOME}/Trash/"

echo "Deleting logs"
sudo rm -r /var/log/*

if pacman -Q apache >> /dev/null; then
  sudo mkdir /var/log/httpd
fi
