# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=~/.config/emacs/bin:~/.local/bin:$PATH
export EDITOR='nvim'

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
alias pinst='yay -S'
alias ppu='yay -Rs'
alias pup='yay -Syu'
alias pinstalled='yay -Q'
pinsearch() {
  yay -Q | grep "$@"
}
alias psearch='yay -F'
alias porphan='yay -Qtdq'
alias pclean='yay -Sc && porphan | yay -Rns -'
alias listaur="yay -Qqem"

# yay
alias yinst='yay -S'

# nix
alias nup='sudo nixos-rebuild switch'
alias nclean='sudo nix-collect-garbage -d'

# other
alias l='exa --all --long --header --icons --git --group-directories-first --color-scale'
alias clr='clear'
alias cllr='clear && l'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias man='batman'
alias vim='nvim'
alias RGB='ls -laR / | lolcat'
alias killonclick="xprop | grep "PID" | awk '{print $3}' | xargs kill"
PS1='[\u@\h \W]\$'

if [ -n "$PS1" ]; then
  (cat ~/.cache/wal/sequences &)
  fortune | pokemonsay
  eval "$(starship init bash)"
fi
