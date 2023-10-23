#!/usr/bin/env bash

if pgrep "waybar" > /dev/null; then
  pgrep "waybar" | xargs kill
  # Launch waybar
  echo "waybar launches..."
  waybar &
else
  # Launch waybar
  echo "waybar launches..."
  waybar &
fi
