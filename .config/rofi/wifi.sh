#!/bin/bash

options="off\non"

choice=$(echo -e $options | rofi -dmenu -l 2 -p "Wi-Fi")

nmcli radio wifi $choice
