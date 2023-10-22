#!/bin/sh

if pgrep "gammastep" > /dev/null; then
  exit
else
  /usr/bin/pipewire & /usr/bin/pipewire-pulse & /usr/bin/wireplumber
fi
