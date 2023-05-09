#!/bin/bash

# setting themes

THEME="Catppuccin-Mocha-Standard-Pink-Dark"
ICONS="Papirus-Dark"
FONT="CodeNewRoman Nerd Font 11"
CURSOR="Dracula-cursors"

SCHEMA='gsettings set org.gnome.desktop.interface'

apply-themes() {
    ${SCHEMA} gtk-theme "$THEME"
    ${SCHEMA} icon-theme "$ICONS"
    ${SCHEMA} cursor-theme "$CURSOR"
    ${SCHEMA} font-name "$FONT"
}
