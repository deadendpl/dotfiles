#!/usr/bin/env bash

touchpad_conf='Section "InputClass"
        Identifier "touchpad"
        MatchIsTouchpad "on"
        Driver "libinput"
        Option "Tapping" "on"
        Option "TappingButtonMap" "lrm"
        Option "NaturalScrolling" "off"
        Option "ScrollMethod" "twofinger"
EndSection'

if [ -e "/etc/X11/xorg.conf.d/90-touchpad.conf" ]; then
    echo "It seems you already have touchpad configuration. No changes have been made."
    exit
else
    echo "$touchpad_conf" | sudo tee /etc/X11/xorg.conf.d/90-touchpad.conf > /dev/null
    echo "The changes have been apllied. After reboot touchpad should work."
    read -p "Press Enter to exit"
fi
