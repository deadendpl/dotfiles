#!/bin/bash

cd ../dependencies/bspwm/
./arch.sh
./aur.sh


cd ../../home/.config
mv * ~/.config
