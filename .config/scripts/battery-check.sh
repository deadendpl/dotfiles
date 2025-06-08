#!/usr/bin/env sh

battery_level=$(upower -i "$(upower -e | grep "BAT")" | \
                  grep "percentage" | awk '{print 0+$2}')
battery_state=$(upower -i "$(upower -e | grep "BAT")" | \
                  grep "state" | awk '{print $2}')

if test "$battery_level" -le 20 && \
   test "$battery_state" == "discharging"; then
  notify-send -i "battery-low" "Battery is at ${battery_level}%." \
              "Please, charge your device!"
fi
