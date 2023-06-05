#!/bin/bash

# Terminate already running bar instances
killall -q waybar

# Launch waybar
echo "waybar launches..."

waybar &
