#!/bin/bash

cd ../dependencies/hypr/
./pacman.sh
./aur.sh


cd ../../home/.config
mv * ~/.config
