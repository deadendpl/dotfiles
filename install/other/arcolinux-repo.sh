#!/bin/bash

repo_lines="[arcolinux_repo_3party]
SigLevel = Required TrustAll
Server = https://arcolinux.github.io/arcolinux_repo_3party/\$arch"

# Backup the original pacman.conf file
sudo cp /etc/pacman.conf /etc/pacman.conf.bak

# Add the repository lines to the end of pacman.conf
sudo bash -c "echo '$repo_lines' >> /etc/pacman.conf"

sudo pacman -Syyu
