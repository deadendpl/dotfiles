#!/usr/bin/env bash

chosen=$(echo -e "󰍃 Logout\n Shutdown\n Reboot\n󰤄 Suspend\n󰉁 Hibernate" | rofi -config ${XDG_CONFIG_HOME}/rofi/config-bare.rasi -theme-str 'window {width: 12%;}' -dmenu -l 5 -i -p Power)

case $chosen in
  "󰍃 Logout")
    pkill -KILL -u $(whoami)
    ;;
  " Shutdown")
    systemctl poweroff
    ;;
  " Reboot")
    systemctl reboot
    ;;
  "󰤄 Suspend")
    systemctl suspend
    ;;
  "󰉁 Hibernate")
    systemctl hibernate
    ;;
esac
