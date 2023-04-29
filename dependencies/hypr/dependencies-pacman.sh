#!/bin/sh

sudo pacman -S $(cat dependencies-hypr.txt | cut -d' ' -f1)
