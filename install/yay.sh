#!/usr/bin/env sh

if $(pacman -Q git >> /dev/null); then
  git clone https://aur.archlinux.org/yay-bin
  cd yay-bin
  makepkg -si
else
  sudo pacman -S --noconfirm git
  git clone https://aur.archlinux.org/yay-bin
  cd yay-bin
  makepkg -si
  cd ..
  rm -rf yay-bin/
fi
