#!/usr/bin/env sh

if pgrep "gammastep" > /dev/null; then
  pkill gammastep
  light -S $(cat /tmp/past-brightness)
else
  light -G > /tmp/past-brightness
  light -S 1
  gammastep -O 2200 2 &
fi
