#!/usr/bin/env sh

if pgrep "waybar" > /dev/null; then
  pkill "waybar"
fi

# Launch waybar
echo "waybar launches..."
waybar &
