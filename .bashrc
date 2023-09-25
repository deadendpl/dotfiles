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
alias psearch='yay -Ss'
alias porphan='yay -Qtdq'
alias pclean='yay -Sc && porphan'
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


# pywal stuff for hyprland
if [ -n "$PS1" ]; then
  (cat ~/.cache/wal/sequences &)
  fortune | pokemonsay
  eval "$(starship init bash)"
fi

pyrice() {

wal-tile() {
  wal -n -i "$@"
  killall swaybg
  swaybg -m fill -i "$(< "${HOME}/.cache/wal/wal")" &
}

walls_dir="$HOME/Pictures/bg"
cd $walls_dir
wall=$(ls $walls_dir | shuf -n 1 | xargs realpath)

wal-tile $wall
wpg -ns $wall


waybar_css="$HOME/.config/waybar/style.css"

# Get the line containing "@import"
old_line=$(grep "@import" "$waybar_css")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    wal_filename=$(realpath $HOME/.cache/wal/colors-waybar.css)
    new_line="@import url('$wal_filename');"
    sed -i "s|$old_line|$new_line|" "$waybar_css"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

qute_start_css="$HOME/.config/qutebrowser/start/styles.css"

# Get the line containing "@import"
old_line=$(grep "@import" "$waybar_css")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    wal_filename=$(realpath $HOME/.cache/wal/colors-waybar.css)
    new_line="@import url('$wal_filename');"
    sed -i "s|$old_line|$new_line|" "$waybar_css"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

zathurarc="$HOME/.config/zathura/zathurarc"

old_line=$(grep "include" "$zathurarc")

# Check if the old line exists in the file
if [ -n "$old_line" ]; then
    # Replace the line using sed
    wal_zathura=$(realpath $HOME/.cache/wal/colors-zathurarc)
    new_line="include $wal_zathura"
    sed -i "s|$old_line|$new_line|" "$zathurarc"
    echo "Line replaced successfully!"
else
    echo "Old line not found in the file."
fi

$HOME/.config/scripts/hypr/waybar-start.sh
$HOME/.config/mako/update-theme.sh

notify-send "New rice applied"
}
