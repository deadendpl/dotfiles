#!/usr/bin/env sh

TITLE=$(playerctl metadata --format "{{title}}")
REST=$(playerctl metadata --format "{{artist}} - {{album}}")

notify-send -i "juk" "$TITLE" "$REST"
