#!/bin/sh

yay -S --noconfirm pywal \
                   python-inotify-simple \
                   python-psutil \
                   python-prctl \
                   python-daemon \
                   wpgtk \
                   gtk-theme-flat-color-git

wpg-install.sh -g
