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



function pyrice

function wal-tile
    wal -n -i $argv
    pkill -x swaybg
    swaybg -m fill -i (cat $HOME/.cache/wal/wal) &
end

set walls_dir "$HOME/Pictures/bg"
cd $walls_dir
set wall (ls $walls_dir | shuf -n 1 | xargs realpath)

wal-tile $wall
wpg -ns $wall

set waybar_css "$HOME/.config/waybar/style.css"

# Get the line containing "@import"
set old_line (grep "@import" "$waybar_css")

# Check if the old line exists in the file
if test -n "$old_line"
    # Replace the line using sed
    set wal_filename (realpath $HOME/.cache/wal/colors-waybar.css)
    set new_line "@import url('$wal_filename');"
    sed -i "s|$old_line|$new_line|" "$waybar_css"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
end

set qute_start_css "$HOME/.config/qutebrowser/start/styles.css"

# Get the line containing "@import"
set old_line (grep "@import" "$qute_start_css")

# Check if the old line exists in the file
if test -n "$old_line"
    # Replace the line using sed
    set wal_filename (realpath $HOME/.cache/wal/colors-waybar.css)
    set new_line "@import url('$wal_filename');"
    sed -i "s|$old_line|$new_line|" "$qute_start_css"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
end

set zathurarc "$HOME/.config/zathura/zathurarc"

set old_line (grep "include" "$zathurarc")

# Check if the old line exists in the file
if test -n "$old_line"
    # Replace the line using sed
    set wal_zathura (realpath $HOME/.cache/wal/colors-zathurarc)
    set new_line "include $wal_zathura"
    sed -i "s|$old_line|$new_line|" "$zathurarc"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
end

$HOME/.config/scripts/hypr/waybar-start.sh
$HOME/.config/mako/update-theme.sh

notify-send "New rice applied"

end