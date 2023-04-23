#!/bin/bash

mkdir -p ~/Pictures/drac-walls

cd ~/Pictures

git clone https://github.com/helpotters/dracula-wallpapers.git

cd dracula-wallpapers/illustrations/cat-and-bats

cp dracula-cat-282a36.png ~/Pictures/drac-walls

cd ../galaxy

cp dracula-galaxy-282a36.png ~/Pictures/drac-walls

cd ../trees-and-mountains

cp dracula-mnt-282a36.png ~/Pictures/drac-walls
cp dracula-mnt-6272a4.png ~/Pictures/drac-walls

cd ../../waves

cp dracula-waves-44475a.png ~/Pictures/drac-walls

cd ~/Pictures

rm -rf dracula-wallpapers

git clone https://github.com/aynp/dracula-wallpapers.git

cd dracula-wallpapers/Art

cp * ~/Pictures/drac-walls

cd "../Programming Languages"

cp * ~/Pictures/drac-walls
