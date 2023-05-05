#!/bin/bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
polybar-msg cmd quit

# Launch Polybar
polybar mybar --config=~/.config/polybar/config.ini | tee -a /tmp/polybar.log & disown

echo "Polybar launched..."
