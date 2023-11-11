#!/usr/bin/env bash

read -p "This works on vanilla Arch Linux. If you're running different distro then backup your data. Otherwise it may break your system."

git clone https://gitlab.com/imnotpua/grub_gtg

cd grub_gtg

sudo bash ./install.sh

cd ../

rm -rf grub_gtg
