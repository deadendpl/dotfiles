#!/bin/bash

gtk_theme="Catppuccin-Latte-Standard-Pink-Light"
icon_theme="Papirus-Light"

gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme"
gsettings set org.gnome.desktop.interface icon-theme "$icon_theme"
