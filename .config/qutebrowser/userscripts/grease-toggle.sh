#!/usr/bin/env bash

grease_dir=${XDG_CONFIG_HOME}/qutebrowser/greasemonkey
scripts=$(ls $grease_dir)

option=$(printf '%s\n' "${scripts[@]}" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -dmenu -i -p "Option:")

# Get the file extension
extension="${option##*.}"

if [ "$extension" == "js" ]; then
  # Add .disabled extension
  mv "${grease_dir}/$option" "${grease_dir}/${option}.disabled"
elif [ "$extension" == "disabled" ]; then
  # Remove .disabled extension
  base_name="${option%.disabled}"
  mv "${grease_dir}/$option" "${grease_dir}/$base_name"
else
  echo "File does not have the expected extensions (.js or .disabled)"
fi

echo "greasemonkey-reload" >> "$QUTE_FIFO"
