#!/bin/bash

file_path="$HOME/.config/gtk-4.0/"

cd $file_path

rm -rf *

# You need to give sudo permissions because the script will copy gtk4 theme from /usr/share/themes/Dracula .
# The script will copy gtk4 theme to ~/.config/gtk-4.0 .

gksudo cp /usr/share/themes/Catppuccin-Latte-Standard-Pink-Light/gtk-4.0/* ./
