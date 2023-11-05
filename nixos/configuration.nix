{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

boot = {
  loader.efi.canTouchEfiVariables = true;

  loader.grub.enable = true;
  loader.grub.device = "nodev";
  loader.grub.efiSupport = true;

supportedFilesystems = [ "ntfs" ];

kernelPackages = pkgs.linuxKernel.packages.linux_zen;
};

networking.hostName = "lenovo-nixos";
networking.networkmanager.enable = true;
networking.firewall.enable = false;

hardware.bluetooth.enable = true;

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

i18n.extraLocaleSettings = {
  LC_ADDRESS = "pl_PL.UTF-8";
  LC_IDENTIFICATION = "pl_PL.UTF-8";
  LC_MEASUREMENT = "pl_PL.UTF-8";
  LC_MONETARY = "pl_PL.UTF-8";
  LC_NAME = "pl_PL.UTF-8";
  LC_NUMERIC = "pl_PL.UTF-8";
  LC_PAPER = "pl_PL.UTF-8";
  LC_TELEPHONE = "pl_PL.UTF-8";
  LC_TIME = "pl_PL.UTF-8";
};

# Enable the X11 windowing system.
services.xserver = {
  enable = true;
  # Enable touchpad support (enabled default in most desktopManager).
  libinput.enable = true;
  # sddm configuration
  displayManager.sddm = {
    enable = true;
    theme = "${import ./sddm-theme.nix { inherit pkgs; }}";
  };
};

# choosing simple greeter for lightdm
# services.xserver.displayManager.lightdm.greeters.tiny.enable = true;


# for 32-bit stuff (like wine)
# hardware.opengl.driSupport32Bit = true;

programs = {
  hyprland.enable = true;
  light.enable = true;

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
mtr.enable = true;
gnupg.agent = {
  enable = true;
  enableSSHSupport = true;
};

fish.enable = true;
};

users.defaultUserShell = pkgs.fish;
services.gnome.gnome-keyring.enable = true;

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
  createHome = true;
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

services.flatpak.enable = false;

# List packages installed in system profile. To search, run:
# $ nix search wget
environment.systemPackages = with pkgs; [
  # (import ./cp-p.nix)
  # cli utils
  (import ./cp-p.nix { inherit pkgs; })
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

  # for sddm
  libsForQt5.qt5.qtquickcontrols2  
  libsForQt5.qt5.qtgraphicaleffects

  # desktop
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

  # some dev stuff
  gnumake
  cmake
  gcc
  libtool

  # service things
  polkit_gnome
  blueberry
  dracula-theme
  networkmanager_dmenu
  gammastep
  pulseaudio
  pavucontrol
  papirus-icon-theme
  swaybg
  swayimg
  swaynotificationcenter
  pcmanfm
  light
  syncthing
  libnotify
  gnome.file-roller
  bitwarden
  bitwarden-cli

  # qutebrowser
  qutebrowser
  python311Packages.inotify-simple
  python311Packages.psutil
  python311Packages.python-daemon

  emacs29
  # this is for installing elisp packages from nix repos instead of normal elisp repos
  # (pkgs.emacsWithPackagesFromUsePackage {
  #     package = pkgs.emacsGit;  # replace with pkgs.emacsPgtk, or another version if desired.
  #     config = path/to/your/config.el;
  #     # config = path/to/your/config.org; # Org-Babel configs also supported

  #     # Optionally provide extra packages not in the configuration file.
  #     extraEmacsPackages = epkgs: [
  #       epkgs.use-package;
  #     ];

  #     # Optionally override derivations.
  #     override = epkgs: epkgs // {
  #       somePackage = epkgs.melpaPackages.somePackage.overrideAttrs(old: {
  #          # Apply fixes here
  #       });
  #     };
  #   })

  # i love how you can specify retroarch cores here
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
  (nerdfonts.override { fonts = [ "CodeNewRoman" "JetBrainsMono" "Ubuntu" "Go-Mono" ]; })
];

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

system = {
  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  copySystemConfiguration = false;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  stateVersion = "23.05"; # Did you read the comment?
};

}
