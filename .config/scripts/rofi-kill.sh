#!/usr/bin/env sh

ps --no-headers f -A -o pid,comm | rofi -config "${XDG_CONFIG_HOME}/rofi/config-bare.rasi" -dmenu -p "Choose process" | awk '{print $1}' | xargs kill
