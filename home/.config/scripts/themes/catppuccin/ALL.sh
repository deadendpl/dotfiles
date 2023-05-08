#!/bin/bash

cd ~/.config/scripts/themes/catppuccin

./alacritty.sh
./doom-emacs.sh
./dunst-bspwm.sh
./dunst-hypr.sh
./foot.sh
./gtk3.sh
./gtk4.sh
./polybar.sh
./qutebrowser.sh
./waybar.sh
./wezterm.sh

echo "It's recommended to logout of your machine re reboot."

read -p "Press Enter to exit"
