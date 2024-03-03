#!/usr/bin/env bash
termux-change-repo
termux-setup-storage
pkg install --yes emacs sqlite fish eza git openssh openssh-sftp-server iproute2 wget
chsh -s fish

# linking directories
rm -rf ~/.config/ ~/.termux/
ln -s $PREFIX/../home/.dotfiles/.config $PREFIX/../home/
ln -s $PREFIX/../home/.dotfiles/termux/.termux $PREFIX/../home/

# setting up the font
cd ~/.termux
wget "https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/JetBrainsMono/Ligatures/Regular/JetBrainsMonoNerdFontMono-Regular.ttf"
mv JetBrainsMonoNerdFontMono-Regular.ttf font.ttf
