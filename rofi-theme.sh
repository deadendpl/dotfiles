#!/bin/bash

cd $HOME

git clone https://github.com/deadendpl/dracula-rofi-theme.git

cd dracula-rofi-theme

mkdir -p $HOME/.local/share/rofi/themes

cp dracula.rasi $HOME/.local/share/rofi/themes

echo 'Now choose dracula theme.'

rofi-theme-selector

rm -rf $HOME/dracula-rofi-theme/
