#!/usr/bin/env bash
sudo -v

if pacman -Q bluez >> /dev/null; then
  echo "Bluetooth is set up."
else
  sudo pacman -S --noconfirm bluez
  sudo systemctl enable bluetooth
fi

if pacman -Q pulseaudio >> /dev/null; then
  sudo pacman -Rc --noconfirm pulseaudio pulseaudio-alsa pulseaudio-bluetooth
  sudo pacman -S --noconfirm pipewire pipewire-pulse pipewire-audio
fi

if pacman -Q sddm >> /dev/null; then
  echo "It seems SDDM is installed."
else
  echo "Installing SDDM"
  sudo pacman -S --noconfirm sddm qt5-quickcontrols qt5-quickcontrols2 qt5-graphicaleffects
  sudo systemctl enable sddm
fi

if [ -e "/usr/share/sddm/themes/win7welcomesddm/" ]; then
  echo "It seems the theme is intalled."
else
  git clone https://github.com/AudacityXD62/win7welcomesddm.git
  sudo mv win7welcomesddm /usr/share/sddm/themes/

  sddm_file_path="/usr/lib/sddm/sddm.conf.d/default.conf"

  old_line=$(grep "Current=" $sddm_file_path)

  # Check if the old line exists in the file
  if [ -n "$old_line" ]; then
    # Replace the line using sed
    new_line='Current=win7welcomesddm'
    sudo sed -i "s|$old_line|$new_line|" "$sddm_file_path"
    echo "SDDM theme applied successfully!"
  else
    echo "SDDM theme not applied successfully!"
  fi
fi

sudo pacman-key --recv-key 3056513887B78AEB --keyserver keyserver.ubuntu.com
sudo pacman-key --lsign-key 3056513887B78AEB
sudo pacman -U --noconfirm 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-keyring.pkg.tar.zst'
sudo pacman -U --noconfirm 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-mirrorlist.pkg.tar.zst'

sudo bash -c "echo -e \"\n[chaotic-aur]\" >> \"/etc/pacman.conf\""
sudo bash -c "echo -e \"Include = /etc/pacman.d/chaotic-mirrorlist\" >> \"/etc/pacman.conf\""

if pacman -Q hyprland >> /dev/null; then
  echo "Normal packages are installed."
else
  sudo pacman -Syyu --noconfirm sway \
                                htop \
                                papirus-icon-theme \
                                waybar \
                                eza \
                                qt5-wayland \
                                swaybg \
                                blueberry \
                                swaync \
                                wdisplays \
                                wl-clipboard \
                                grim \
                                slurp \
                                bat-extras \
                                fish \
                                starship \
                                pcmanfm-gtk3 \
                                file-roller \
                                p7zip \
                                unrar \
                                zathura \
                                zathura-pdf-mupdf \
                                gammastep \
                                lolcat \
                                ttf-ubuntu-nerd \
                                ttf-jetbrains-mono-nerd \
                                noto-fonts-emoji \
                                pavucontrol \
                                qt5ct \
                                foot \
                                networkmanager \
                                qutebrowser \
                                python-adblock \
                                mpv \
                                mpv-mpris \
                                polkit-gnome \
                                grimshot \
                                dracula-cursors-git \
                                light \
                                chafa \
                                ripgrep \
                                fzf \
                                hyprpicker-git \
                                swayimg \
                                emacs-wayland \
                                lf \
                                stow \
                                expac \
                                python-tldextract \
                                python-pynacl \
                                fastfetch \
                                git \
                                tree-sitter \
                                udiskie \
                                swaylock
                                # hyprland \
                                # neovim \
                                # otf-codenewroman-nerd \
fi

if pacman -Q yay >> /dev/null; then
  echo "It seems yay is installed."
else
  git clone https://aur.archlinux.org/yay-bin
  cd yay-bin
  makepkg -si --noconfirm
  cd ..
  rm -rf yay-bin/
fi

if pacman -Q rofi-lbonn-wayland-git >> /dev/null; then
  echo "AUR essential packages are installed."
else
  yay -S --noconfirm networkmanager-dmenu-git \
                     rofi-bluetooth-git \
                     ctpv-git \
                     cp-p-git \
                     rofi-lbonn-wayland-git
                     # clipboard \
                     # pokemonsay-newgenerations-git \
                     # fortune-mod-vimtips \
                     # udiskie-dmenu-git \
                     # sioyek \
                     # krabby-bin \
                     # dracula-gtk-theme \
                     # catppuccin-gtk-theme-mocha \
                     # catppuccin-gtk-theme-latte \
                     # bitwarden-rofi-git \
fi

if pacman -Q wpgtk >> /dev/null; then
  echo "It seems pywal packages are installed."
else
  echo "Installing pywal packages."
  yay -S --noconfirm python-pywal16 \
                     python-inotify-simple \
                     python-psutil \
                     python-prctl \
                     python-daemon \
                     python-haishoku \
                     chaotic-aur/qt5-styleplugins \
                     chaotic-aur/qt6gtk2 \
                     python-zombie-imp \
                     gradience \
                     wpgtk \
                     gtk-theme-flat-color-git
                     # chaotic-aur/adw-gtk3

  # using pip like this can break stuff, be careful
  sudo pip install yapsy --break-system-packages
  sudo pip install anyascii --break-system-packages
  sudo pip install modern_colorthief --break-system-packages

  wpg-install.sh -g
fi

if pacman -Q picard >> /dev/null; then
  echo "Optional packages are installed."
else
  yay -S --noconfirm appimagelauncher-bin \
                     keepassxc \
                     syncthing \
                     gnome-disk-utility \
                     freefilesync \
                     picard \
                     rsgain \
                     nicotine+ \
                     xdg-ninja \
                     otf-ipaexfont \
                     playerctl \
                     ntfs-3g
                     # syncthing-gtk
fi

if [ -e "/etc/X11/xorg.conf.d/90-touchpad.conf" ]; then
  echo "It seems you already have touchpad configuration. No changes have been made."
else
  touchpad_conf='Section "InputClass"
        Identifier "touchpad"
        MatchIsTouchpad "on"
        Driver "libinput"
        Option "Tapping" "on"
        Option "TappingButtonMap" "lrm"
        Option "NaturalScrolling" "off"
        Option "ScrollMethod" "twofinger"
EndSection'

  sudo mkdir -p /etc/X11/xorg.conf.d
  sudo touch /etc/X11/xorg.conf.d/90-touchpad.conf
  echo "$touchpad_conf" | sudo tee /etc/X11/xorg.conf.d/90-touchpad.conf > /dev/null
  echo "The changes have been applied. After reboot touchpad should work."
fi

if pacman -Q texlive-bin >> /dev/null; then
  echo "Latex is setup."
else
  sudo pacman -S --noconfirm texlive-bin texlive-binextra texlive-latexrecommended texlive-latexextra texlive-plaingeneric
  sudo texconfig rehash
  texhash
fi

git clone https://gitlab.com/imnotpua/grub_gtg

cd grub_gtg

echo "YOU NEED TO TYPE FONT SIZE AND THEME DIRECTORY"
sudo bash ./install.sh

cd ../
rm -rf grub_gtg

chsh -s "$(command -v fish)"
curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install -o "omf"
fish omf --noninteractive
fish -c "omf install foreign-env"
rm omf

xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop

gsettings set org.gnome.desktop.interface gtk-theme "FlatColor"
gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
gsettings set org.gnome.desktop.interface cursor-theme "Dracula-cursors"
gsettings set org.gnome.desktop.interface font-name "Ubuntu Nerd Font 10"
gsettings set org.gnome.desktop.interface document-font-name "Ubuntu Nerd Font 10"
gsettings set org.gnome.desktop.interface monospace-font-name "JetBrainsMono NFM 10"
gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"

yay -S --noconfirm xdg-user-dirs
xdg-user-dirs-update

# enabling syncthing service
systemctl --user enable syncthing
