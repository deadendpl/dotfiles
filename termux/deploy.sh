#!/usr/bin/env bash
termux-change-repo
termux-setup-storage
pkg install emacs sqlite fish eza git openssh openssh-sftp-server iproute2
chsh -s fish
