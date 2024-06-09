#!/usr/bin/env bash

# this script will copy your current neovim cofiguration to root account

src=${XDG_CONFIG_HOME}/nvim
destination=/root/.config/nvim

sudo mkdir -p $destination
sudo cp -r $src/* $destination
