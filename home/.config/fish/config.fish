if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source

set PATH ~/.config/emacs/bin $PATH
export EDITROR='vim'

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
alias asearch='apt search'

# pacman
alias pinst='sudo pacman -S'
alias ppu='sudo pacman -Rs'
alias pup='sudo pacman -Syu'
alias pinstalled='pacman -Q'
alias psearch='pacman -Ss'
alias pclean='sudo pacman -Sc'

alias listaur="pacman -Qqem"

# yay
alias yinst='yay -S'

# other
alias ll='exa --all --long --header --icons --git --group-directories-first'
alias clr='clear'
alias cllr='clear && ll'
alias grep='grep --color=auto'
alias man='batman'
alias vim='nvim'
alias myeyes='gammastep -O 2200 2>/dev/null'
alias RGB='ls -laR / | lolcat'

colorscript random

end
