#!/usr/bin/env bash

gtk_theme="Dracula"
icon_theme="Papirus-Dark"

gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme"
gsettings set org.gnome.desktop.interface icon-theme "$icon_theme"
