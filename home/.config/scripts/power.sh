#!/bin/bash

chosen=$(echo -e "󰍃 Logout\n Shutdown\n Reboot\n󰤄 Suspend" | $launcher -dmenu -l 4 -i -b -p Power)

if [[ $chosen = "󰍃 Logout" ]]; then
	pkill -KILL -u $(whoami)
elif [[ $chosen = " Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = " Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "󰤄 Suspend" ]]; then
	systemctl suspend
fi
