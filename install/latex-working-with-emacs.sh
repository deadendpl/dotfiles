#!/usr/bin/env sh
sudo pacman -S texlive-bin texlive-binextra texlive-latexrecommended texlive-latexextra texlive-plaingeneric
sudo texconfig rehash
texhash
