#!/usr/bin/env bash

lines=$(udiskie-info -a -o "{ui_label}" | wc -l)
choice=$(udiskie-info -a -o "{ui_label}" | rofi -dmenu -l $lines -p "Disks")

choice=$(echo $choice | awk '{print $1}' | sed 's/:/ /g')

udiskie-umount -d $choice
