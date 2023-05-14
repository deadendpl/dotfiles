#!/bin/bash

gtk_theme="Catppuccin-Latte-Standard-Pink-Light"

# Apply the GTK theme using gsettings
gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme"
