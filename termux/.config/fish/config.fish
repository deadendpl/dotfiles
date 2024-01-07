export TERM=xterm-256color
set -gx EDITOR 'emacsclient -c -t -a "nvim"'

# apt
alias pinst='pkg install'
alias ppu='pkg purge'
alias pautopu='pkg autopurge'
alias pup='pkg upgrade'
alias pupd='pkg update'
alias pupg='pkg upgrade'
alias pinstalled='pkg list --installed'
alias psearch='pkg search'

alias clr='clear'
alias l='eza --all --long --header --icons --git --group-directories-first --color-scale all'
alias vim="emacsclient -c -t -a ''"
