#!/bin/bash

if pgrep -x gammastep > /dev/null; then
    killall gammastep
else
    gammastep -O 2200 2>/dev/null &
fi
