#!/bin/sh

yay -S $(cat aur.txt | cut -d' ' -f1)
