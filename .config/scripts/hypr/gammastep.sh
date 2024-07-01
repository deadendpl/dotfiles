#!/usr/bin/env sh

if pgrep "gammastep" > /dev/null; then
  pkill gammastep
else
  gammastep -O 2200 2 &
fi
