#!/usr/bin/env bash

if pgrep "waybar" > /dev/null; then
  pkill "waybar"
fi

# Launch waybar
echo "waybar launches..."
waybar &
