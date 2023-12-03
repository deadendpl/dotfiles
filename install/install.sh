#!/usr/bin/env bash
sudo pacman -S --noconfirm bluez
sudo systemctl enable bluetooth

sudo pacman -Rc --noconfirm pulseaudio pulseaudio-alsa pulseaudio-bluetooth
sudo pacman -S --noconfirm pipewire pipewire-pulse pipewire-audio

sudo pacman -S --noconfirm sddm qt5-quickcontrols qt5-quickcontrols2 qt5-graphicaleffects
sudo systemctl enable sddm
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

repo_lines="
[arcolinux_repo_3party]
SigLevel = Required TrustAll
Server = https://arcolinux.github.io/arcolinux_repo_3party/\$arch"

# Backup the original pacman.conf file
sudo cp /etc/pacman.conf /etc/pacman.conf.bak

# Add the repository lines to the end of pacman.conf
sudo bash -c "echo '$repo_lines' >> /etc/pacman.conf"

sudo pacman -Syyu

sudo pacman -S --noconfirm hyprland \
                           htop \
                           papirus-icon-theme \
                           waybar \
                           eza \
                           qt5-wayland \
                           swaybg \
                           blueberry \
                           mako \
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
                           neovim \
                           gammastep \
                           lolcat \
                           otf-codenewroman-nerd \
                           ttf-ubuntu-nerd \
                           ttf-go-nerd \
                           ttf-jetbrains-mono-nerd \
                           noto-fonts-emoji \
                           pavucontrol \
                           qt5ct \
                           foot \
                           networkmanager \
                           gksu \
                           qutebrowser \
                           mpv \
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
                           udiskie

if $(pacman -Q yay >> /dev/null); then
  echo "yay is installed"
else
  git clone https://aur.archlinux.org/yay-bin
  cd yay-bin
  makepkg -si --noconfirm
  cd ..
  rm -rf yay-bin/
fi

yay -S --noconfirm networkmanager-dmenu-git \
                   clipboard \
                   rofi-bluetooth-git \
                   ctpv-git \
                   cp-p-git \
                   rofi-lbonn-wayland-git \
                   pokemonsay-newgenerations-git \
                   fortune-mod-vimtips \
                   # krabby-bin \
                   # dracula-gtk-theme
                   # catppuccin-gtk-theme-mocha
                   # catppuccin-gtk-theme-latte
                   # bitwarden-rofi-git \
                   # udiskie-dmenu-git

yay -S --noconfirm pywal \
                   python-inotify-simple \
                   python-psutil \
                   python-prctl \
                   python-daemon \
                   python-haishoku \
                   wpgtk \
                   gtk-theme-flat-color-git

wpg-install.sh -g

yay -S --noconfirm appimagelauncher-bin \
                   keepassxc \
                   syncthing \
                   gnome-disk-utility
                   # syncthing-gtk

touchpad_conf='Section "InputClass"
        Identifier "touchpad"
        MatchIsTouchpad "on"
        Driver "libinput"
        Option "Tapping" "on"
        Option "TappingButtonMap" "lrm"
        Option "NaturalScrolling" "off"
        Option "ScrollMethod" "twofinger"
EndSection'

if [ -e "/etc/X11/xorg.conf.d/90-touchpad.conf" ]; then
  echo "It seems you already have touchpad configuration. No changes have been made."
  exit
else
  sudo mkdir -p /etc/X11/xorg.conf.d
  sudo touch /etc/X11/xorg.conf.d/90-touchpad.conf
  echo "$touchpad_conf" | sudo tee /etc/X11/xorg.conf.d/90-touchpad.conf > /dev/null
  echo "The changes have been applied. After reboot touchpad should work."
fi

sudo pacman -S --noconfirm texlive-bin texlive-binextra texlive-latexrecommended texlive-latexextra texlive-plaingeneric
sudo texconfig rehash
texhash

git clone https://gitlab.com/imnotpua/grub_gtg

cd grub_gtg

echo "YOU NEED TO TYPE FONT SIZE AND THEME DIRECTORY"
sudo bash ./install.sh

cd ../
rm -rf grub_gtg

xdg-mime default org.pwmt.zathura.desktop application/pdf
xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
xdg-mime default pcmanfm.desktop inode/directory
