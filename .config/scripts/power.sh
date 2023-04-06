#!/bin/bash
#
# Requirements:
# - rofi
# - systemd, but you can replace the commands for OpenRC or anything else

chosen=$(echo -e "󰍃 Logout\n Shutdown\n Reboot\n󰤄 Suspend" | rofi -dmenu -i -p Power)
# Info about some states are available here:
# https://www.freedesktop.org/software/systemd/man/systemd-sleep.conf.html#Description

if [[ $chosen = "󰍃 Logout" ]]; then
	bspc quit
elif [[ $chosen = " Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = " Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "󰤄 Suspend" ]]; then
	systemctl suspend
fi
