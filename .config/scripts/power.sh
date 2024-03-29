#!/usr/bin/env bash

chosen=$(echo -e "󰍃 Logout\n Shutdown\n Reboot\n󰤄 Suspend\n󰉁 Hibernate" | rofi -config ~/.config/rofi/config-bare.rasi -theme-str 'window {width: 12%;}' -dmenu -l 5 -i -p Power)

if [[ $chosen = "󰍃 Logout" ]]; then
	pkill -KILL -u $(whoami)
elif [[ $chosen = " Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = " Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "󰤄 Suspend" ]]; then
	systemctl suspend
elif [[ $chosen = "󰉁 Hibernate" ]]; then
	systemctl hibernate
fi
