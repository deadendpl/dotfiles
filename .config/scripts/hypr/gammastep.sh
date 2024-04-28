#!/usr/bin/env sh

if pgrep "gammastep" > /dev/null; then
  pgrep "gammastep" | xargs kill
else
  gammastep -O 2200 2 > /dev/null &
fi
