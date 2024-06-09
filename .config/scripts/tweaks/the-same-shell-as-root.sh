#!/usr/bin/env bash

read -rp "Which shell do you choose? [bash/fish] " choice

if [[ $choice == "fish" ]]; then
  sudo chsh -s $(command -q -v fish)
  sudo cp -rf ${XDG_CONFIG_HOME}/fish/ /root/.config/fish
  sudo cp -rf ${XDG_CONFIG_HOME}/starship.toml /root/.config/starship.toml
elif [[ $choice == "bash" ]]; then
  sudo chsh -s $(command -q -v bash)
  sudo cp -rf ~/.bashrc /root/.bashrc
  sudo cp -rf ${XDG_CONFIG_HOME}/starship.toml /root/.config/starship.toml
else
  echo "You chose wrong shell"
  exit
fi

read -p "Press Enter to exit"
