#!/usr/bin/env bash
if $(pacman -Q bluez >> /dev/null); then
  echo "Bluetooth is set up."
else
  sudo pacman -S --noconfirm bluez
  sudo systemctl enable bluetooth
fi

if $(pacman -Q pulseaudio >> /dev/null); then
  sudo pacman -Rc --noconfirm pulseaudio pulseaudio-alsa pulseaudio-bluetooth
  sudo pacman -S --noconfirm pipewire pipewire-pulse pipewire-audio
fi

if $(pacman -Q sddm >> /dev/null); then
  echo "It seems SDDM is installed."
else
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

repo_lines="
[arcolinux_repo_3party]
SigLevel = Required TrustAll
Server = https://arcolinux.github.io/arcolinux_repo_3party/\$arch"

# Backup the original pacman.conf file
sudo cp /etc/pacman.conf /etc/pacman.conf.bak

# Add the repository lines to the end of pacman.conf
sudo bash -c "echo '$repo_lines' >> /etc/pacman.conf"

sudo pacman -Syyu

if $(pacman -Q hyprland >> /dev/null); then
  echo "Normal packages are installed."
else
  sudo pacman -S --noconfirm hyprland \
                             htop \
                             papirus-icon-theme \
                             waybar \
                             eza \
                             qt5-wayland \
                             swaybg \
                             blueberry \
                             swaync \
                             wdisplays-git \
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
                             emacs \
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
                             # neovim \
                             # otf-codenewroman-nerd \
fi

if $(pacman -Q yay >> /dev/null); then
  echo "It seems yay is installed."
else
  git clone https://aur.archlinux.org/yay-bin
  cd yay-bin
  makepkg -si --noconfirm
  cd ..
  rm -rf yay-bin/
fi

if $(pacman -Q rofi-lbonn-wayland-git >> /dev/null); then
  echo "AUR essential packages are installed."
else
  yay -S --noconfirm networkmanager-dmenu-git \
                     clipboard \
                     rofi-bluetooth-git \
                     ctpv-git \
                     cp-p-git \
                     rofi-lbonn-wayland-git \
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

if $(pacman -Q wpgtk >> /dev/null); then
  echo "It seems pywal packages are installed."
else
  yay -S --noconfirm pywal-16-colors \
                     python-inotify-simple \
                     python-psutil \
                     python-prctl \
                     python-daemon \
                     python-haishoku \
                     wpgtk \
                     gtk-theme-flat-color-git \
                     qt5-styleplugins \
                     qt6gtk2

  wpg-install.sh -g
fi

if $(pacman -Q picard >> /dev/null); then
  echo "Optional packages are installed."
else
  yay -S --noconfirm appimagelauncher-bin \
                     keepassxc \
                     syncthing \
                     gnome-disk-utility \
                     freefilesync \
                     picard \
                     rsgain \
                     nicotine+
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

if $(pacman -Q texlive-bin >> /dev/null); then
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

sudo bash -c "echo -e \"\nQT_QPA_PLATFORMTHEME=gtk2\" >> \"/etc/environment\""
sudo bash -c "echo -e \"\nCALIBRE_USE_SYSTEM_THEME=1\" >> \"/etc/environment\"" # this is not necessary if you don't use calibre

xdg-mime default org.pwmt.zathura.desktop application/pdf
xdg-mime default org.pwmt.zathura.desktop application/epub+zip
xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
xdg-mime default pcmanfm.desktop inode/directory

gsettings set org.gnome.desktop.interface gtk-theme "FlatColor"
gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
gsettings set org.gnome.desktop.interface cursor-theme "Dracula-cursors"
gsettings set org.gnome.desktop.interface font-name "Ubuntu Nerd Font 10"
gsettings set org.gnome.desktop.interface document-font-name "Ubuntu Nerd Font 10"
gsettings set org.gnome.desktop.interface monospace-font-name "JetBrainsMono NFM 10"
gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
