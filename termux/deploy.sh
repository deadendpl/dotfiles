#!/usr/bin/env bash
termux-change-repo
termux-setup-storage
pkg install --yes emacs sqlite fish eza git openssh iproute2 wget stow curl which
chsh -s fish

# linking directories
rm -rf ~/.config/
cd ~/.dotfiles
stow .

# setting up the font
mkdir ~/.termux
wget "https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/JetBrainsMono/Ligatures/Regular/JetBrainsMonoNerdFontMono-Regular.ttf" -O ~/.termux/font.ttf

# importing .profile
curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install | fish
fish -c "omf install foreign-env"

passwd
