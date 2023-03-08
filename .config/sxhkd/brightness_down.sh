#!/bin/bash
#Get the argument
fir_argument=":)"
# Get the current brightness level
brightness=$(xrandr --current --verbose | grep eDP-1 -A5 | grep -o -P '(?<=Brightness: ).*')
# Decrease/increse the brightness by 5%
if [ "$fir_argument" = "decrease" ]; then
new_brightness=$(echo "$brightness + 0.1" | bc)
else
new_brightness=$(echo "$brightness - 0.1" | bc)
fi
# Check if the new brightness level is above 1
if [ "$(echo "$new_brightness > 1" | bc)" -eq 1 ]; then
new_brightness=1
fi
# Check if the new brightness level is below 0
if [ "$(echo "$new_brightness < 0" | bc)" -eq 1 ]; then
new_brightness=0
fi
# Set the new brightness level
xrandr --output eDP-1 --brightness $new_brightness
