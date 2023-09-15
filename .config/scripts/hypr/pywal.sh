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
$HOME/.config/scripts/hypr/waybar-start.sh
$HOME/.config/mako/update-theme.sh

#wal-tile $walls_dir
# wpg -ns $walls_dir
