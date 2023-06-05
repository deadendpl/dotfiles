if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source

set PATH ~/.config/emacs/bin $PATH
export EDITOR='vim'

set fish_greeting
fish_vi_key_bindings

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
function pinsearch
    pacman -Q | grep $argv
end
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
alias RGB='ls -laR / | lolcat'

colorscript random

end
