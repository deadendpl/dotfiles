#!/bin/sh

sudo pacman -S strace

yay -S $(cat aur.txt | cut -d' ' -f1)
