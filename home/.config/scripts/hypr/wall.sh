#!/bin/bash

walls_dir="$HOME/Pictures/drac-walls"

cd "$walls_dir" || exit 1  # Change directory and exit script if it fails

wall=$(ls | shuf -n 1)

swaybg -m fill -i "$walls_dir/$wall" &

