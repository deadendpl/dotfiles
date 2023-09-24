#!/usr/bin/env bash

wal-tile() {
  wal -n -i "$@"
  killall swaybg
  swaybg -m fill -i "$(< "${HOME}/.cache/wal/wal")" &
}

walls_dir="$HOME/Pictures/bg"
cd $walls_dir
wall=$(ls $walls_dir | shuf -n 1 | xargs realpath)

wal-tile $wall
wpg -ns $wall


waybar_css="$HOME/.config/waybar/style.css"

# Get the line containing "@import"
old_line=$(grep "@import" "$waybar_css")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    wal_filename=$(realpath $HOME/.cache/wal/colors-waybar.css)
    new_line="@import url('$wal_filename');"
    sed -i "s|$old_line|$new_line|" "$waybar_css"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

qute_start_css="$HOME/.config/qutebrowser/start/styles.css"

# Get the line containing "@import"
old_line=$(grep "@import" "$qute_start_css")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    wal_filename=$(realpath $HOME/.cache/wal/colors.css)
    new_line="@import url('$wal_filename');"
    sed -i "s|$old_line|$new_line|" "$waybar_css"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

zathurarc="$HOME/.config/zathura/zathurarc"

old_line=$(grep "include" "$zathurarc")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    wal_zathura=$(realpath $HOME/.cache/wal/colors-zathurarc)
    new_line="include $wal_zathura"
    sed -i "s|$old_line|$new_line|" "$zathurarc"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

$HOME/.config/scripts/hypr/waybar-start.sh
$HOME/.config/mako/update-theme.sh
emacsclient --eval '(load-theme real-theme t)'

notify-send "New rice applied"
