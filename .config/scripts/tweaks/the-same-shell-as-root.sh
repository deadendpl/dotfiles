#!/usr/bin/env bash

read -rp "Which shell do you choose? [bash/fish] " choice

if [[ $choice == "fish" ]]; then
  sudo chsh -s $(which fish)
  sudo cp -rf ~/.config/fish/ /root/.config/fish
  sudo cp -rf ~/.config/starship.toml /root/.config/starship.toml

elif [[ $choice == "bash" ]]; then
  sudo chsh -s $(which bash)
  sudo cp -rf ~/.bashrc /root/.bashrc
  sudo cp -rf ~/.config/starship.toml /root/.config/starship.toml

else
  echo "You chose wrong shell"
  exit
fi

read -p "Press Enter to exit"
