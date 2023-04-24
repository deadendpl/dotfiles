if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source

set PATH ~/.config/emacs/bin $PATH
export EDITOR='vim'

set fish_greeting

## aliases

# apt
alias ainst='sudo apt install'
alias apu='sudo apt purge'
alias aautopu='sudo apt autopurge'
alias aup='sudo apt update && sudo apt upgrade'
alias aupd='sudo apt update'
alias aupg='sudo apt upgrade'
alias ainstalled='apt list --installed'

# pacman
alias pinst='sudo pacman -S'
alias ppu='sudo pacman -Rs'
alias pup='sudo pacman -Syu'
alias pupd='sudo pacman -Sy'
alias pupg='sudo pacman -u'
alias pinstalled='pacman -Q'

# other
alias ll='exa --all --long --header --icons --git --group-directories-first'
alias clr='clear'
alias cllr='clear && ll'
alias grep='grep --color=auto'

colorscript random

end
