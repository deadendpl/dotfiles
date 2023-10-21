#!/usr/bin/env bash

# you should execute that script after installing all dependencies
# it sets some apps to be defaults for some files

xdg-mime default org.pwmt.zathura.desktop application/pdf
xdg-mime default swayimg.desktop image/*
xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
