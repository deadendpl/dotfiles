#!/usr/bin/env bash
termux-change-repo
termux-setup-storage
pkg install --yes emacs sqlite fish eza git openssh openssh-sftp-server iproute2
chsh -s fish

rm -rf ~/.config/
ln -s $PREFIX/../home/.dotfiles/termux/.config $PREFIX/../home/.config
ln -s $PREFIX/../home/.dotfiles/termux/.termux $PREFIX/../home/.termux
