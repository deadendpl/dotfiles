#!/bin/sh

yay -S $(cat dependencies-hypr.txt | cut -d' ' -f1)
