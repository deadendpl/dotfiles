# -*- mode: sh -*-
if status is-interactive
  function fish_greeting
    fortune | pokemonsay
  end
  starship init fish | source
end


set PATH ~/.config/emacs/bin $PATH
set PATH ~/.local/bin $PATH

set EDITOR emacsclient -t -a "nvim"
set VISUAL emacsclient -c -a "emacs"

set fish_greeting
fish_vi_key_bindings

# going to last directory from lf
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

# pacman and yay
alias pinst='yay -S'
alias ppu='yay -Rs'
alias pup='yay -Syu'
alias pinstalled='yay -Q'
function pinsearch
  yay -Q | grep $argv
end
alias psearch='yay -F'
alias porphan='yay -Qtdq'
alias pclean='yay -Sc && porphan | yay -Rns -'
alias listaur="yay -Qqem"

# nix
alias ninst='nix-env -iA'
alias nup='sudo cp ~/.dotfiles/nixos/* /etc/nixos/ && sudo nixos-rebuild switch'
alias nclean='sudo nix-collect-garbage -d'

# other
alias cp='cp -v'
alias l='exa --all --long --header --icons --git --group-directories-first --color-scale'
alias lf='lfcd'
alias clr='clear'
alias cllr='clear && l'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias man='batman'
alias v='emacsclient -t -a "nvim"'
alias vim='nvim'
alias RGB='ls -laR / | lolcat'
alias demacs='emacs --daemon'
alias remacs='killall emacs;killall emacs && emacs --daemon'
alias rickroll='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'
alias killonclick="xprop | grep "PID" | awk '{print $3}' | xargs kill"
alias myip='curl "https://wtfismyip.com/text"'
alias swaylock='~/.cache/wal/swaylock.sh'
