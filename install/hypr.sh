#!/bin/bash

# arcolinux repo
read -rp "Are you on ArcoLinux? [y/n] " arco

if [[ "$arco" == "n" ]]; then
  other/arcolinux-repo.sh
fi

cd other/dependencies/hypr
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

printf "\nIN ORDER TO USE THE RICE YOU NEED TO CREATE A HYPRLAND SESSION YOURSELF.\nIn order to do that you need to create a wayland session file for example \"sudo nano /usr/share/wayland-sessions/session_name.desktop\".\nAnd there put:\n\n[Desktop Entry]\nType=Application\nName=based hyprland\nExec=/home/<your_username>/.config/scripts/hypr/wrappedhl\n\nIf I or you didn't break anything it should work now\n"
