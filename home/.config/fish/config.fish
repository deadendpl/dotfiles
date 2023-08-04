if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source

set PATH ~/.config/emacs/bin $PATH
export EDITOR='nvim'

set fish_greeting
fish_vi_key_bindings

# importing tty colors
source ~/.config/tty-colors/fish/dracula

# going to las directory from lf
function lfcd
    set tmp (mktemp)
    # `command` is needed in case `lfcd` is aliased to `lf`
    command lf -last-dir-path=$tmp $argv
    if test -f "$tmp"
        set dir (cat $tmp)
        rm -f $tmp
        if test -d "$dir"
            if test "$dir" != (pwd)
                cd $dir
            end
        end
    end
end

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

# nix
alias nup='sudo nixos-rebuild switch'
alias nclean='sudo nix-collect-garbage -d'

# other
alias l='exa --all --long --header --icons --git --group-directories-first'
alias lf='lfcd'
alias clr='clear'
alias cllr='clear && l'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias man='batman'
alias vim='nvim'
alias RGB='ls -laR / | lolcat'
alias killonclick="xprop | grep "PID" | awk '{print $3}' | xargs kill"

colorscript random

end
