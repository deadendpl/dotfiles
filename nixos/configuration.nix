{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

boot.loader.efi.canTouchEfiVariables = true;

boot.loader.grub.enable = true;
boot.loader.grub.device = "nodev";
boot.loader.grub.efiSupport = true;

boot.supportedFilesystems = [ "ntfs" ];

boot.kernelPackages = pkgs.linuxKernel.packages.linux_zen;

networking.hostName = "lenovo-nixos";
networking.networkmanager.enable = true;
networking.firewall.enable = false;

hardware.bluetooth.enable = true;

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
programs.mtr.enable = true;
programs.gnupg.agent = {
  enable = true;
  enableSSHSupport = true;
};

services.gnome.gnome-keyring.enable = true;

time.timeZone = "Europe/Warsaw";

# Select internationalisation properties.
# i18n.defaultLocale = "en_US.UTF-8";
console = {
#   font = "Lat2-Terminus16";
  keyMap = "pl";
#   useXkbConfig = true; # use xkbOptions in tty.
};

# Configure keymap in X11
services.xserver.layout = "pl";
# services.xserver.xkbOptions = "eurosign:e,caps:escape";

# Enable the X11 windowing system.
services.xserver.enable = true;

# choosing simple greeter for lightdm
services.xserver.displayManager.lightdm.greeters.tiny.enable = true;

# for 32-bit stuff (like wine)
# hardware.opengl.driSupport32Bit = true;

# Enable touchpad support (enabled default in most desktopManager).
services.xserver.libinput.enable = true;

programs.hyprland.enable = true;

programs.light.enable = true;

# rtkit is optional but recommended
security.rtkit.enable = true;
services.pipewire = {
  enable = true;
  alsa.enable = true;
  alsa.support32Bit = true;
  pulse.enable = true;
  # If you want to use JACK applications, uncomment this
  #jack.enable = true;
};

# Define a user account. Don't forget to set a password with ‘passwd’.
users.users.oliwier = {
  isNormalUser = true;
  extraGroups = [ "wheel" "video" "networkmanager" ];
  packages = with pkgs; [
  ];
};

environment.etc."xdg/user-dirs.defaults".text = ''
  DESKTOP=Desktop
  DOWNLOAD=Downloads
  TEMPLATES=Templates
  PUBLICSHARE=Public
  DOCUMENTS=Documents
  MUSIC=Music
  PICTURES=Pictures
  VIDEOS=Videos
'';

users.defaultUserShell = pkgs.fish;
programs.fish.enable = true;

services.flatpak.enable = false;

# List packages installed in system profile. To search, run:
# $ nix search wget
environment.systemPackages = with pkgs; [
  wget
  lolcat
  htop
  neofetch
  fish
  bash
  fzf
  git
  exa
  starship
  bat
  bat-extras.batman
  bat-extras.prettybat
  bat-extras.batgrep
  lf
  fortune
  cowsay
  pokemonsay
  ctpv
  chafa
  killall
  unrar
  ripgrep
  clipboard-jh

  hyprland
  foot
  mako
  neovim
  waybar
  rofi-wayland
  wl-clipboard
  sway-contrib.grimshot
  xdg-utils
  xdg-user-dirs
  gnome.gnome-tweaks
  lxappearance-gtk2
  mpv
  wpgtk
  pywal

  gnumake
  cmake
  gcc
  libtool

  polkit_gnome
  blueberry
  dracula-theme
  networkmanager_dmenu
  gammastep
  pulseaudio
  pavucontrol
  papirus-icon-theme
  swaybg
  pcmanfm
  light
  syncthing
  libnotify
  gnome.file-roller
  bitwarden
  bitwarden-cli

  qutebrowser
  python311Packages.inotify-simple
  python311Packages.psutil
  python311Packages.python-daemon

  emacs29
  (retroarch.override {
    cores = with libretro; [
      ppsspp
      parallel-n64
      snes9x
      swanstation
      melonds
    ];
  })
];

nixpkgs.config.allowUnfree = true;

services.emacs.defaultEditor = true;

# in unstable: fonts.packages = with pkgs; [
fonts.fonts = with pkgs; [
  (nerdfonts.override { fonts = [ "CodeNewRoman" "Ubuntu" "Go-Mono" ]; })
];

environment.etc."xdg/gtk-2.0/gtkrc".text = ''
    gtk-theme-name = "Dracula"
  '';

environment.etc."xdg/gtk-3.0/settings.ini".text = ''
    [Settings]
    gtk-theme-name = "Dracula"
  '';

# setting up xdg desktop portal
services.dbus.enable = true;
xdg.portal = {
  enable = true;
  wlr.enable = true;
  # gtk portal needed to make gtk apps happy
  extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
};

# gnome polkit
systemd = {
 user.services.polkit-gnome-authentication-agent-1 = {
   description = "polkit-gnome-authentication-agent-1";
   wantedBy = [ "graphical-session.target" ];
   wants = [ "graphical-session.target" ];
   after = [ "graphical-session.target" ];
   serviceConfig = {
       Type = "simple";
       ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
       Restart = "on-failure";
       RestartSec = 1;
       TimeoutStopSec = 10;
     };
 };
  extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';
};

# Copy the NixOS configuration file and link it from the resulting system
# (/run/current-system/configuration.nix). This is useful in case you
# accidentally delete configuration.nix.
system.copySystemConfiguration = true;

# This value determines the NixOS release from which the default
# settings for stateful data, like file locations and database versions
# on your system were taken. It's perfectly fine and recommended to leave
# this value at the release version of the first install of this system.
# Before changing this value read the documentation for this option
# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
system.stateVersion = "23.05"; # Did you read the comment?
}
