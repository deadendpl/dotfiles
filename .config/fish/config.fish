if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source

set PATH ~/.config/emacs/bin $PATH
export EDITOR='vim'

set fish_greeting

## aliases

# apt
alias inst='sudo apt install'
alias pu='sudo apt purge'
alias autopu='sudo apt autopurge'
alias up='sudo apt update && sudo apt upgrade'
alias upd='sudo apt update'
alias upg='sudo apt upgrade'
alias ainstalled='apt list --installed'

# other
alias ll='exa --all --long --header --icons --git --group-directories-first'
alias clr='clear'
alias cllr='clear && ll'
alias grep='grep --color=auto'

colorscript random

end
