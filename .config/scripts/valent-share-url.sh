#!/usr/bin/env sh

CLIPBOARD=$(wl-paste)

if [ -n "$CLIPBOARD" ]; then
  valent "$CLIPBOARD"
else
  valent "$(rofi -config "${XDG_CONFIG_HOME}/rofi/config-bare.rasi" -dmenu -p "URL" -l 0)"
fi
