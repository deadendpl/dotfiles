#!/usr/bin/env bash

choice=$(udiskie-info -a -o "{ui_label}" | rofi -dmenu)

choice=$(echo $choice | awk '{print $1}' | sed 's/:/ /g')

udiskie-umount $choice
