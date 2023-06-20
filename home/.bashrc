# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=~/.config/emacs/bin:$PATH
export EDITOR='vim'

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
pinsearch() {
  pacman -Q | grep "$@"
}
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
alias ip='ip -color=auto'
alias man='batman'
alias vim='nvim'
alias RGB='ls -laR / | lolcat'
PS1='[\u@\h \W]\$'

colorscript random

eval "$(starship init bash)"
