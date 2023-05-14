#!/bin/bash

gtk_theme="Catppuccin-Mocha-Standard-Pink-Dark"

# Apply the GTK theme using gsettings
gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme"
