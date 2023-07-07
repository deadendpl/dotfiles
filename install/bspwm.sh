#!/usr/bin/env bash

pwd=$(pwd)

# arcolinux repo
read -rp "Are you on ArcoLinux? [y/n] " arco

if [[ "$arco" == "n" ]]; then
  other/arcolinux-repo.sh
  cd $pwd
fi

# dracula walls
read -rp "Do you want to download some Dracula wallpapers? [y/n] " drac

if [[ "$drac" == "y" ]]; then
  other/walls/dracula-walls.sh
  cd $pwd
fi

# catppuccin walls
read -rp "Do you want to download some Catppuccin wallpapers? [y/n] " cat

if [[ "$cat" == "y" ]]; then
  other/walls/catppuccin-walls.sh
  cd $pwd
fi

echo "Now dependencies will be installed."

cd other/dependencies/bspwm
./pacman.sh
./aur.sh

# doom emacs
read -rp "Do you want to install Doom Emacs? [y/n] " emacs

if [[ "$emacs" == "y" ]]; then
  ../../doom-emacs.sh
else
  echo "Doom Emacs will not be installed (you will have default emacs config)"
fi

cd ../../../../home/.config

# Check if there are any directories present in ~/.config
if [[ $(ls -d ~/.config/*/ 2>/dev/null) ]]; then
  # Prompt the user before overwriting folders in ~/.config
  read -rp "WARNING: ~/.config directory is not empty. Proceed with overwriting existing files? [y/N] " confirm
  if [[ ! "$confirm" =~ [Yy] ]]; then
    echo "Operation aborted by user." >&2
    exit 1
  fi
fi

# Move all directories from the source directory to the destination directory
cp -rf * ~/.config/

echo "Now you need to log out of your machine or reboot"
