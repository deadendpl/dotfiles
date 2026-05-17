#!/usr/bin/env bash
DOTFILES_INSTALL_DIR=$(pwd)

sudo -v

if pacman -Q bluez >> /dev/null; then
  echo "Bluetooth is set up."
else
  sudo pacman -S --noconfirm --needed bluez
  sudo systemctl enable bluetooth
fi

sudo -v

if pacman -Q pulseaudio >> /dev/null; then
  sudo pacman -Rc --noconfirm pulseaudio pulseaudio-alsa pulseaudio-bluetooth
  sudo pacman -S --noconfirm --needed pipewire pipewire-pulse pipewire-audio
fi

sudo -v

if pacman -Q sddm >> /dev/null; then
  echo "It seems SDDM is installed."
else
  echo "Installing SDDM"
  sudo pacman -S --noconfirm --needed sddm qt5-quickcontrols \
                                           qt5-quickcontrols2 \
                                           qt5-graphicaleffects
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

sudo -v

sudo pacman-key --recv-key 3056513887B78AEB --keyserver keyserver.ubuntu.com
sudo pacman-key --lsign-key 3056513887B78AEB
sudo pacman -U --noconfirm --needed 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-keyring.pkg.tar.zst'
sudo pacman -U --noconfirm --needed 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-mirrorlist.pkg.tar.zst'

if ! grep chaotic-aur /etc/pacman.conf >> /dev/null; then
  sudo bash -c "echo -e \"\n[chaotic-aur]\" >> \"/etc/pacman.conf\""
  sudo bash -c "echo -e \"Include = /etc/pacman.d/chaotic-mirrorlist\" >> \"/etc/pacman.conf\""
fi

sudo -v

if pacman -Q sway >> /dev/null; then
  echo "Normal packages are installed."
else
  sudo pacman -Syyu --noconfirm --needed sway \
                                         htop \
                                         papirus-icon-theme \
                                         waybar \
                                         eza \
                                         qt5-wayland \
                                         swaybg \
                                         blueman \
                                         swaync \
                                         wdisplays \
                                         wl-clipboard \
                                         grim \
                                         slurp \
                                         bat-extras \
                                         fish \
                                         starship \
                                         pcmanfm \
                                         file-roller \
                                         7zip \
                                         unrar \
                                         gammastep \
                                         ttf-ubuntu-nerd \
                                         ttf-jetbrains-mono-nerd \
                                         noto-fonts-emoji \
                                         pavucontrol \
                                         qt5ct \
                                         foot \
                                         networkmanager \
                                         python-adblock \
                                         mpv \
                                         mpv-mpris \
                                         polkit-gnome \
                                         sway-contrib \
                                         dracula-cursors-git \
                                         light \
                                         chafa \
                                         ripgrep \
                                         fzf \
                                         swayimg \
                                         emacs-wayland \
                                         stow \
                                         expac \
                                         python-tldextract \
                                         python-pynacl \
                                         fastfetch \
                                         git \
                                         tree-sitter \
                                         udiskie \
                                         swaylock \
                                         fcron \
                                         xorg-xwayland \
                                         unzip \
                                         wl-clip-persist \
                                         glide-browser-bin \
                                         rofi \
                                         sioyek-git \
                                         seahorse \
                                         sbcl \
                                         webp-pixbuf-loader # for swaybg to work with webp
                                         # zathura \
                                         # zathura-pdf-mupdf \
                                         # chaotic-aur/zen-browser-bin \
                                         # qutebrowser \
                                         # lf \
                                         # hyprland \
                                         # hyprpicker-git \
                                         # neovim \
                                         # otf-codenewroman-nerd \
fi

sudo -v

if pacman -Q yay >> /dev/null; then
  echo "It seems yay is installed."
else
  git clone https://aur.archlinux.org/yay-bin
  cd yay-bin
  makepkg -si --noconfirm --needed
  cd ..
  rm -rf yay-bin/
fi

if pacman -Q rofi-bluetooth-git >> /dev/null; then
  echo "AUR essential packages are installed."
else
  yay -S --noconfirm --needed networkmanager-dmenu-git \
                              rofi-bluetooth-git \
                              cp-p-git \
                              # ctpv-git \
                              # clipboard \
                              # pokemonsay-newgenerations-git \
                              # fortune-mod-vimtips \
                              # udiskie-dmenu-git \
                              # krabby-bin \
                              # dracula-gtk-theme \
                              # catppuccin-gtk-theme-mocha \
                              # catppuccin-gtk-theme-latte \
                              # bitwarden-rofi-git \
fi

sudo -v

if pacman -Q wpgtk >> /dev/null; then
  echo "It seems pywal packages are installed."
else
  echo "Installing pywal packages."
  yay -S --noconfirm --needed python-pywal16 \
                              python-haishoku \
                              qt5-styleplugins \
                              chaotic-aur/qt6gtk2 \
                              gradience \
                              python-anyascii \
                              python-material-color-utilities \
                              python-zombie-imp \
                              wpgtk \
                              # gtk-theme-flat-color-git
                              # python-inotify-simple \
                              # python-psutil \
                              # python-prctl \
                              # python-daemon \
                              # chaotic-aur/adw-gtk3

  # using pip like this can break stuff, be careful
  # sudo pip install yapsy --break-system-packages
  # sudo pip install anyascii --break-system-packages
  # sudo pip install modern_colorthief --break-system-packages

  wpg-install.sh -g
fi

sudo -v

if pacman -Q picard >> /dev/null; then
  echo "Optional packages are installed."
else
  yay -S --noconfirm --needed appimagelauncher \
                              keepassxc \
                              syncthing \
                              gnome-disk-utility \
                              picard \
                              rsgain \
                              nicotine+ \
                              xdg-ninja \
                              otf-ipaexfont \
                              playerctl \
                              ntfs-3g \
                              # freefilesync \
                              # syncthing-gtk
fi

sudo -v

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

sudo -v

if pacman -Q texlive-bin >> /dev/null; then
  echo "Latex is setup."
else
  sudo pacman -S --noconfirm --needed texlive-bin \
                                      texlive-binextra \
                                      texlive-latexrecommended \
                                      texlive-latexextra \
                                      texlive-plaingeneric
  sudo texconfig rehash
  texhash
fi

sudo -v

cd /tmp/

git clone https://gitlab.com/imnotpua/grub_gtg

cd grub_gtg

echo "YOU NEED TO TYPE FONT SIZE AND THEME DIRECTORY"
sudo bash ./install.sh

sudo -v

cd /tmp/

chsh -s "$(command -v fish)"
curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install -o "omf"
fish omf --noninteractive
fish -c "omf install foreign-env"

sudo -v

FCRON_FILE=$(mktemp)

sudo systemctl enable fcron
# write out current crontab
fcrontab -l > $FCRON_FILE
# echo new cron into cron file
echo "*/5 * * * * sudo -u \$USER DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/\$(id -u \$USER)/bus /home/\$USER/.config/scripts/battery-check.sh" >> $FCRON_FILE
# install new cron file
fcrontab $FCRON_FILE

gsettings set org.gnome.desktop.interface gtk-theme "FlatColor"
gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
gsettings set org.gnome.desktop.interface cursor-theme "Dracula-cursors"
gsettings set org.gnome.desktop.interface font-name "Ubuntu Nerd Font 10"
gsettings set org.gnome.desktop.interface document-font-name "Ubuntu Nerd Font 10"
gsettings set org.gnome.desktop.interface monospace-font-name "JetBrainsMono NFM 10"
gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"

if [ -d ~/.local/share/applications/ ]; then
  mkdir -p ~/.local/share/applications/
fi

echo << EOF
The glide browser will be turned on for a moment.
After 20 seconds it will be closed.
EOF

glide-bin --setDefaultBrowser & disown

sleep 20

pkill glide

basename $(ls ~/.local/share/applications/*Glide*) | xargs xdg-settings set default-web-browser

sudo -v

yay -S --noconfirm --needed xdg-user-dirs
xdg-user-dirs-update

# enabling syncthing service
if pacman -Q syncthing >> /dev/null; then
  systemctl --user enable syncthing
fi

sudo -v

sudo usermod -aG video,audio,input $(whoami)

cd ../tools/pyrice-in-common-lisp/
if ! pacman -Q sbcl >> /dev/null; then
  sudo pacman --noconfirm --needed -S sbcl
fi
echo "Note that you will need to exit SBCL manually."
curl https://beta.quicklisp.org/quicklisp.lisp -o ~/quicklisp.lisp
sbcl --load ~/quicklisp.lisp \
     --eval "(quicklisp-quickstart:install)" \
     --eval "(ql:add-to-init-file)" \
     --eval "(quit)"
make copy

cd $DOTFILES_INSTALL_DIR/rofi-bookmarks
makepkg -si --noconfirm --needed

cd $DOTFILES_INSTALL_DIR/gradience

yay -S --noconfirm --needed python-anyascii \
                            python-yapsy \
                            python-material-color-utilities \
                            python-zombie-imp

makepkg -si --noconfirm --needed

read -p "Want to have an example wallpaper downloaded to make pyrice work? [y/n]: " -n 1 -r
echo
if [[ "$REPLY" = "y" ]]; then
  mkdir -p ~/Pictures/bg/
  curl -s -L "https://picsum.photos/1920/1080" --output ~/Pictures/bg/example-image
fi

cat << EOF
Installation has been completed.

Some programs need to be manually opened to be configured.
Emacs for example upon logging into sway will install its packages in
the background but may require manual intervention.
Also note that pyrice will look for images in ~/Pictures/bg/ so either
change the command in sway config or put your wallpapers there.
Also note that gradience GUI will not work as gradience is abandoned software.
The gradience CLI still works though which is what pyrice can use.
EOF
