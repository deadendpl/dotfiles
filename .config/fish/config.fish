# -*- mode: sh -*-

if set -q TERMUX_VERSION
  # termux config
  set fish_greeting
  export TERM=xterm-256color

  alias pinst='pkg install'
  alias ppu='pkg uninstall'
  alias pautopu='pkg autoclean'
  alias pup='pkg upgrade'
  alias pupd='pkg update'
  alias pupg='pkg upgrade'
  alias pinstalled='pkg list --installed'
  alias psearch='pkg search'

  alias l='eza --all --long --header --icons --git --group-directories-first --color-scale all'
  alias clr='clear'
  alias cllr='clear && l'
  alias v='emacsclient -c -a ""'
else
  # normal config
  if status is-interactive
    set fish_greeting
    # function fish_greeting
    #   echo "Why won't you do it in eshell?" | pokemonsay
    # end
  end
  starship init fish | source


  set PATH ~/.config/emacs/bin $PATH
  set PATH ~/.local/bin $PATH

  set EDITOR emacsclient -t -a "nvim"
  set VISUAL emacsclient -c -a "emacs"
  # fish_vi_key_bindings

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
  # alias ainst='sudo apt install'
  # alias apu='sudo apt purge'
  # alias aautopu='sudo apt autopurge'
  # alias aup='sudo apt update && sudo apt upgrade'
  # alias aupd='sudo apt update'
  # alias aupg='sudo apt upgrade'
  # alias ainstalled='apt list --installed'
  # alias asearch='apt search'

  # pacman and yay on Arch
  alias pinst='yay -S'
  alias ppu='yay -Rs'
  alias pup='yay -Syu'
  alias pinstalled='yay -Q'
  function pinsearch
    yay -Q | grep $argv
  end
  alias psearch='yay -F'
  alias porphan='yay -Qtdq'
  alias pclean='yay --noconfirm -Sc && porphan | yay --noconfirm -Rns -'
  alias listaur='yay -Qqem'

  # nix
  alias ninst='nix-env -iA'
  alias nup='sudo cp ~/.dotfiles/nixos/* /etc/nixos/ && sudo nixos-rebuild switch'
  alias nclean='sudo nix-collect-garbage -d'

  # other
  alias connect='nmcli device wifi connect'
  alias l='eza --all --long --header --icons --git --group-directories-first --color-scale all'
  alias lf='lfcd'
  alias cp='cp-p'
  alias clr='clear'
  alias cllr='clear && l'
  alias fetch='fastfetch'
  alias grep='grep --color=auto'
  alias ip='ip --color=auto'
  alias man='batman'
  alias mv='mv-p'
  alias v='emacsclient -t -a "nvim"'
  alias vim='nvim'
  alias RGB="cat /dev/urandom | tr -dc 'a-z A-Z' | lolcat"
  alias demacs='emacs --daemon'
  alias remacs='pkill emacs && emacs --daemon'
  alias rickroll='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'
  alias myip='curl "https://wtfismyip.com/text"'
  alias swaylock='~/.cache/wal/swaylock.sh'
  alias pi='ping wp.pl'

  if set -q INSIDE_EMACS
    or set -q NVIM
      fish_default_key_bindings
  else
    fish_vi_key_bindings
    set fish_cursor_replace_one underscore
    set fish_cursor_insert line
    if string match -rq 'wezterm|foot|tmux' -- $TERM
      set fish_vi_force_cursor true
    end
  end

end
