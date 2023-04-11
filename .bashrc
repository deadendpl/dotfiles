# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=~/.config/emacs/bin:$PATH
export EDITOR='vim'

# apt
alias inst='sudo apt install'
alias pu='sudo apt purge'
alias autopu='sudo apt autopurge'
alias up='sudo apt update && sudo apt upgrade'
alias upd='sudo apt update'
alias upg='sudo apt upgrade'
alias ainstalled='apt list --installed'

# other
alias clr='clear'
alias cllr='clear && ll'
alias ll='exa --all --long --header --icons --git'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$'

colorscript random

eval "$(starship init bash)"
