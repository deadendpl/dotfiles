#!/usr/bin/env bash

# Prompt for the volume change using Rofi
choice=$(rofi -config ~/.config/rofi/config-bare.rasi -dmenu  -b -l 0 -p "How much to change the volume by?")

# Check if the choice is a valid number
if [[ $choice =~ ^-?[0-9]+$ ]]; then
  # Format the choice to include the percentage symbol
  if [[ $choice -ge 0 ]]; then
    formatted_choice="+${choice}%"
  else
    formatted_choice="${choice}%"
  fi

  # Adjust the volume using pactl
  pactl set-sink-volume @DEFAULT_SINK@ "$formatted_choice"
else
  echo "Invalid input: $choice"
fi
