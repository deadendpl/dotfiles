#!/bin/sh

sudo pacman -S $(cat dependencies-pacman.txt | cut -d' ' -f1)