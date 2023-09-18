if status is-interactive
  function fish_greeting
    fortune | pokemonsay
  end
  cat ~/.cache/wal/sequences &
  starship init fish | source
end


set PATH ~/.config/emacs/bin $PATH
set PATH ~/.local/bin $PATH

export EDITOR='nvim'

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
alias psearch='yay -Ss'
alias porphan='yay -Qtdq'
alias pclean='yay -Sc && porphan'
alias listaur="yay -Qqem"

# nix
alias nup='sudo nixos-rebuild switch'
alias nclean='sudo nix-collect-garbage -d'

# other
alias l='exa --all --long --header --icons --git --group-directories-first --color-scale'
alias lf='lfcd'
alias clr='clear'
alias cllr='clear && l'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias man='batman'
alias vim='nvim'
alias RGB='ls -laR / | lolcat'
alias killonclick="xprop | grep "PID" | awk '{print $3}' | xargs kill"
