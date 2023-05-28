#!/bin/bash

# the script will download some catppuccin wallpapers

# making wallpapers directory
mkdir -p ~/Pictures/cat-walls

cd ~/Pictures

# cloning wallpaper repo and moving some wallpepers to ~/Pictures/cat-walls
git clone https://github.com/zhichaoh/catppuccin-wallpapers
cd catppuccin-wallpapers/dithered
cp barn.png lighthouse.png marketplace.png ~/Pictures/cat-walls/
cd ../flatppuccin
cp * ~/Pictures/cat-walls/
cd ../gradients/
cp * ~/Pictures/cat-walls/
cd ../mandelbrot/
cp * ~/Pictures/cat-walls/
cd ../minimalistic/
cp * ~/Pictures/cat-walls/
cd ../misc/
cp * ~/Pictures/cat-walls/
cd ../waves/
cp cat-blue-eye.png cat-waves.png ~/Pictures/cat-walls/

# removing wallpaper repo
cd ../..
rm -rf catppuccin-wallpapers
