#!/bin/bash

gtk_theme="Dracula"

# Apply the GTK theme using gsettings
gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme"
