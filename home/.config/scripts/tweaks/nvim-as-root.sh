#!/bin/bash

# this script will copy you current neovim cofiguration to root account

src=$HOME/.config/nvim
destination=/root/.config/nvim

sudo mkdir -p $destination
sudo cp -r $src/* $destination
