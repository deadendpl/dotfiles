#!/bin/bash

chosen=$(echo -e "󰍃 Logout\n Shutdown\n Reboot\n󰤄 Suspend" | $launcher -theme ~/.config/rofi/themes/drac.rasi -theme-str 'window {width: 12%;}' -dmenu -l 4 -i -p Power)

if [[ $chosen = "󰍃 Logout" ]]; then
	pkill -KILL -u $(whoami)
elif [[ $chosen = " Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = " Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "󰤄 Suspend" ]]; then
	systemctl suspend
fi
