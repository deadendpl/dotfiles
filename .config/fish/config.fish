# emacs keybindings
fish_default_key_bindings

fenv source "$HOME"/.profile

alias l='eza -a -l -h --icons --git --group-directories-first --color-scale all'
alias clr='clear'
alias cllr='clear && l'
alias grep='grep --color=auto'
alias ip='ip --color=auto'
alias v='$EDITOR'
alias RGB="cat /dev/urandom | tr -dc 'a-z A-Z' | lolcat"
alias demacs='emacs --daemon'
alias remacs='pkill emacs && emacs --daemon'
alias rickroll='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'
alias myip='curl "https://wtfismyip.com/text"'
alias pi='ping wp.pl'

# nix
if which nixos-rebuild > /dev/null 2>&1
  alias pinst='nix-env -iA'
  alias pup='sudo cp ~/.dotfiles/nixos/* /etc/nixos/ && sudo nixos-rebuild switch'
  alias pclean='sudo nix-collect-garbage -d'
end

if status is-interactive
  set fish_greeting
end

set EDITOR emacsclient -t -a ""
set VISUAL emacsclient -c -a ""

# going to last directory from lf
function lfcd
  set tmp (mktemp)
  # `command` is needed in case `lfcd` is abbred to `lf`
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

# apt
if which apt > /dev/null 2>&1
  alias pinst='sudo apt install'
  alias ppu='sudo apt purge'
  alias pautopu='sudo apt autopurge'
  alias pup='sudo apt update && sudo apt upgrade'
  alias pupd='sudo apt update'
  alias pupg='sudo apt upgrade'
  alias pinstalled='apt list --installed'
  alias psearch='apt search'
end

# pacman and yay on Arch
if which pacman > /dev/null 2>&1
  alias pinst='yay -S'
  alias pinsty='yay -S --noconfirm'
  alias ppu='yay -Rs'
  alias ppuy='yay -Rs --noconfirm'
  alias pup='yay -Syu'
  alias pinstalled='yay -Q'
  function pinsearch
    yay -Q | grep $argv
  end
  alias psearch='yay -F'
  alias porphan='yay -Qtdq'
  alias pclean='yay --noconfirm -Sc && yay -Qtdq | yay --noconfirm -Rns -'
  alias listaur='yay -Qqem'
end

if test -n "$TERMUX_VERSION"
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
else
  starship init fish | source
  alias fetch='fastfetch'
  alias lf='lfcd;pgrep "lf" | xargs kill'
  alias cp='cp-p'
  alias man='batman'
  alias mv='mv-p'
  alias swaylock='~/.cache/wal/swaylock.sh'
  alias connect='nmcli device wifi connect'
end
