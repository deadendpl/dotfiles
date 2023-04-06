#!/bin/sh

yay -S $(cat dependencies-aur.txt | cut -d' ' -f1)
