#!/bin/bash

repo_lines="[arcolinux_repo_3party]
SigLevel = Required DatabaseOptional
Server = https://arcolinux.github.io/arcolinux_repo_3party/\$arch"

# Backup the original pacman.conf file
sudo cp /etc/pacman.conf /etc/pacman.conf.bak

# Add the repository lines to pacman.conf
sudo bash -c "echo \"$repo_lines\" >> /etc/pacman.conf"

# Update Pacman's package lists
sudo pacman -Sy

# Clean the package cache
sudo pacman -Scc --noconfirm

# Refresh the keyring
sudo rm -r /etc/pacman.d/gnupg
sudo pacman-key --init
sudo pacman-key --populate archlinux

# Update the package database
sudo pacman -Sy

# Clean the package cache again
sudo pacman -Scc --noconfirm

echo "ArcoLinux third-party repository has been added to Pacman."
