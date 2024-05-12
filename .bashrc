# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source "$HOME"/.profile

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
alias pclean='yay --noconfirm -Sc && porphan | yay --noconfirm -Rns -'
alias listaur="yay -Qqem"

# yay
alias yinst='yay -S'

# nix
alias ninst='nix-env -iA'
alias nup='sudo nixos-rebuild switch'
alias nclean='sudo nix-collect-garbage -d'

# other
alias cp='cp -v'
alias connect='nmcli device wifi connect'
alias l='exa --all --long --header --icons --git --group-directories-first --color-scale all'
alias clr='clear'
alias cllr='clear && l'
alias fetch='fastfetch'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias man='batman'
alias mv='mv-p'
alias v='$EDITOR'
alias vim='nvim'
alias RGB="cat /dev/urandom | tr -dc 'a-z A-Z' | lolcat"
alias demacs='emacs --daemon'
alias remacs='pkill emacs && emacs --daemon'
alias rickroll='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'
alias myip='curl "https://wtfismyip.com/text"'
# PS1='[\u@\h \W]\$'

if [ -n "$PS1" ]; then
  # fortune | pokemonsay
  eval "$(starship init bash)"
fi
