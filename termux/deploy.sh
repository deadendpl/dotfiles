#!/usr/bin/env bash
termux-change-repo
termux-setup-storage

packages=(
  "emacs" "sqlite" "fish" "eza" "git" "openssh" "iproute2" "wget" "stow"
  "curl" "which"
)

for pkg in "${packages[@]}"; do
  if ! dpkg -s "$pkg" >/dev/null 2>&1; then
    echo "Installing $pkg..."
    pkg install -y "$pkg"
  else
    echo "$pkg is already installed."
  fi
done

chsh -s fish

# linking directories
rm -rf ~/.config/
mkdir ~/.config
cd ~/.dotfiles
stow .

# setting up the font
mkdir ~/.termux
wget "https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/JetBrainsMono/Ligatures/Regular/JetBrainsMonoNerdFontMono-Regular.ttf" -O ~/.termux/font.ttf

# importing .profile
curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install | fish
fish -c "omf install foreign-env"

passwd
