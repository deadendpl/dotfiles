#!/bin/sh

sudo pacman -S $(cat pacman.txt | cut -d' ' -f1)
