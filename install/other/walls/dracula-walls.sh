#!/usr/bin/env bash

# the script will download some dracula wallpapers

# making wallpapers directory
mkdir -p ~/Pictures/drac-walls

cd ~/Pictures

# cloning first wallpaper repo and moving some wallpepers to ~/Pictures/drac-walls
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

# doing the same but with other wallpapers repo
cd ~/Pictures
rm -rf dracula-wallpapers
git clone https://github.com/aynp/dracula-wallpapers.git
cd dracula-wallpapers/Art
cp * ~/Pictures/drac-walls
cd "../Programming Languages"
cp * ~/Pictures/drac-walls

# removing wallpaper repo
cd ../..
rm -rf dracula-wallpapers
