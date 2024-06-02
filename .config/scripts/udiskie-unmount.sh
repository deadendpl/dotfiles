#!/usr/bin/env bash

# this mess is because command substitution replaces new lines with spaces
output=""
while IFS= read -r line; do
  output+="$line"$'\n'
done < <(udiskie-info -a -o "{ui_label}")

# there is one line too much so I decrease it by 1
lines=$(($(echo "$output" | wc -l) - 1))
choice=$(echo "$output" | rofi -dmenu -l $lines -p "Disks")

choice=$(echo $choice | awk '{print $1}' | sed 's/:/ /g')

udiskie-umount -d $choice
