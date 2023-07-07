#!/usr/bin/env bash

dir="$HOME/Documents/books"

if [ ! -d "$dir" ]; then
  mkdir -p "$dir"
fi

cd "$dir"

files=(*)
file_count=${#files[@]}

book=$(printf '%s\n' "${files[@]}" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -l "$file_count" -b -p "Books")

if [[ -n "$book" ]]; then
  zathura "$book"
fi
