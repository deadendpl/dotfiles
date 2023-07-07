#!/usr/bin/env bash

walls_dir="$HOME/Pictures/drac-walls"

cd "$walls_dir" || exit 1  # Change directory and exit script if it fails

wall=$(ls | shuf -n 1)

killall swaybg

swaybg -m fill -i "$walls_dir/$wall" &
