#!/usr/bin/env bash

dir="${HOME}/Documents/books"

if [ ! -d "$dir" ]; then
  mkdir -p "$dir"
fi

cd "$dir"

files=$(ls)
file_count=${#files[@]}

book=$(printf '%s\n' "${files[@]}" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -l "$file_count" -p "Books")

if [[ -n "$book" ]]; then
  zathura "$book"
fi
