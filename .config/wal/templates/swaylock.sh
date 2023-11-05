#!/usr/bin/env bash

color1_value="{color1.strip}"  # Replace with your actual variable names
color2_value="{color2.strip}"
color3_value="{color3.strip}"
color4_value="{color4.strip}"
color5_value="{color5.strip}"
background_value="{background.strip}"

swaylock \
  --image "~/.config/wpg/.current" \
  --indicator-radius 160 \
  --indicator-thickness 20 \
  --inside-color 00000000 \
  --inside-clear-color 00000000 \
  --inside-ver-color 00000000 \
  --inside-wrong-color 00000000 \
  --key-hl-color "$color1_value" \
  --bs-hl-color "$color2_value" \
  --ring-color "$background_value" \
  --ring-clear-color "$color2_value" \
  --ring-wrong-color "$color5_value" \
  --ring-ver-color "$color3_value" \
  --line-uses-ring \
  --line-color 00000000 \
  --font 'JetBrainsMono Nerd Font Mono:style=Thin,Regular 40' \
  --text-color 00000000 \
  --text-clear-color "$color2_value" \
  --text-wrong-color "$color5_value" \
  --text-ver-color "$color4_value" \
  --separator-color 00000000
