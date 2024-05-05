# emacs keybindings
fish_default_key_bindings

fenv source ~/.profile

abbr l 'eza -a -l -h --icons --git --group-directories-first --color-scale all'
abbr clr 'clear'
abbr cllr 'clear && l'
abbr grep 'grep --color=auto'
abbr ip 'ip --color=auto'
abbr v '$EDITOR'
abbr RGB "cat /dev/urandom | tr -dc 'a-z A-Z' | lolcat"
abbr demacs 'emacs --daemon'
abbr remacs 'pkill emacs && emacs --daemon'
abbr rickroll 'curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'
abbr myip 'curl "https://wtfismyip.com/text"'
# when invoked from other programs, fish doesn't know abbrevs
alias pi='ping wp.pl'
abbr pi 'ping wp.pl'

# nix
if which nixos-rebuild > /dev/null 2>&1
  abbr pinst 'nix-env -iA'
  abbr pup 'sudo cp ~/.dotfiles/nixos/* /etc/nixos/ && sudo nixos-rebuild switch'
  abbr pclean 'sudo nix-collect-garbage -d'
end

if status is-interactive
  set fish_greeting
end

set PATH ~/.config/emacs/bin $PATH
set PATH ~/.local/bin $PATH

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
  abbr pinst 'sudo apt install'
  abbr ppu 'sudo apt purge'
  abbr pautopu 'sudo apt autopurge'
  abbr pup 'sudo apt update && sudo apt upgrade'
  abbr pupd 'sudo apt update'
  abbr pupg 'sudo apt upgrade'
  abbr pinstalled 'apt list --installed'
  abbr psearch 'apt search'
end

# pacman and yay on Arch
if which pacman > /dev/null 2>&1
  abbr pinst 'yay -S'
  abbr ppu 'yay -Rs'
  abbr pup 'yay -Syu'
  abbr pinstalled 'yay -Q'
  function pinsearch
    yay -Q | grep $argv
  end
  abbr psearch 'yay -F'
  abbr porphan 'yay -Qtdq'
  abbr pclean 'yay --noconfirm -Sc && porphan | yay --noconfirm -Rns -'
  abbr listaur 'yay -Qqem'
end

if test -n "$TERMUX_VERSION"
  # termux config
  set fish_greeting
  export TERM=xterm-256color

  abbr pinst 'pkg install'
  abbr ppu 'pkg uninstall'
  abbr pautopu 'pkg autoclean'
  abbr pup 'pkg upgrade'
  abbr pupd 'pkg update'
  abbr pupg 'pkg upgrade'
  abbr pinstalled 'pkg list --installed'
  abbr psearch 'pkg search'
else
  starship init fish | source
  abbr fetch 'fastfetch'
  abbr lf 'lfcd;pgrep "lf" | xargs kill'
  abbr cp 'cp-p'
  abbr man 'batman'
  abbr mv 'mv-p'
  abbr swaylock '~/.cache/wal/swaylock.sh'
  abbr connect 'nmcli device wifi connect'
end

# if set -q INSIDE_EMACS
#   or set -q NVIM
#     fish_default_key_bindings
# else
#   fish_vi_key_bindings
#   set fish_cursor_replace_one underscore
#   set fish_cursor_insert line
#   if string match -rq 'wezterm|foot|tmux' -- $TERM
#     set fish_vi_force_cursor true
#   end
# end
