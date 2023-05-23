#!/bin/bash

process="gammastep"

if pgrep -x "$process" > /dev/null; then
    killall gammastep
else
    gammastep -O 2200 2>/dev/null &
fi
