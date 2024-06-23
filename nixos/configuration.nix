{ config, pkgs, ... }:

let
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz;
in
{
  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

boot = {
  tmp.cleanOnBoot = true;
  loader.efi.canTouchEfiVariables = true;

  loader.grub = {
    enable = true;
      device = "nodev";
      efiSupport = true;
      theme = pkgs.fetchFromGitHub { # current as of 11/2023
        owner = "deadendpl";
        repo = "nix-grub_gtg";
        rev = "f0a6eec0993b731562fd44acfe7851aed82179ec";
        sha256 = "1fg3vdqxj7qqpbhl96xhl3175a68bg0c7xbn7q2hspc2272hmh6b";
      };
      # theme = pkgs.fetchFromGitHub { # current as of 11/2023
      #   owner = "shvchk";
      #   repo = "fallout-grub-theme";
      #   rev = "e8433860b11abb08720d7c32f5b9a2a534011bca";
      #   sha256 = "1cf0gd7gziw1j6kilhihpdlna6j1hhvpsgaxsmsqckjmc7igixls";
      # };
  };

  supportedFilesystems = [ "ntfs" ];

  kernelPackages = pkgs.unstable.linuxKernel.packages.linux_zen;
};

nix = {
   settings.experimental-features = [ "nix-command" "flakes" ];
   settings.auto-optimise-store = true;
   gc = {
     automatic = true;
     dates = "2d";
     options = "-d";
   };
 };

networking = {
  hostName = "lenovo-nixos";
  networkmanager.enable = true;
  firewall.enable = false;
};

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
  LC_TIME = "en_US.UTF-8";
};

# Enable the X11 windowing system.
services.xserver = {
  enable = true;
  # Enable touchpad support (enabled default in most desktopManager).
  libinput.enable = true;
  # sddm configuration
  displayManager.sddm = {
    enable = true;
    theme = "${import ./sddm-win7.nix { inherit pkgs; }}";
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
services.udisks2.enable = true;

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
  (import ./cp-p.nix { inherit (pkgs) lib stdenv fetchFromGitHub; })
  wget
  lolcat
  htop
  btop
  unstable.fastfetch
  uwufetch
  fish
  bash
  fzf
  git
  unstable.eza
  starship
  bat
  bat-extras.batman
  bat-extras.prettybat
  bat-extras.batgrep
  unstable.lf
  fortune
  cowsay
  pokemonsay
  ctpv
  chafa
  killall
  unrar
  ripgrep
  fd
  clipboard-jh
  nix-prefetch-git
  stow
  unzip

  # for sddm
  libsForQt5.qt5.qtquickcontrols2
  libsForQt5.qt5.qtgraphicaleffects

  # desktop
  unstable.hyprland
  unstable.hyprpicker
  foot
  # mako
  unstable.neovim
  unstable.waybar
  rofi-wayland
  rofi-bluetooth
  wl-clipboard
  sway-contrib.grimshot
  xdg-utils
  xdg-user-dirs
  gnome.gnome-tweaks
  lxappearance-gtk2
  mpv
  mpvScripts.mpris
  wpgtk
  pywal
  swaybg
  swayimg
  swaynotificationcenter
  # swaylock
  # swaylock-fancy
  gnome.file-roller
  papirus-icon-theme
  dracula-theme
  zathura
  libreoffice-still

  # some dev stuff
  gnumake
  cmake
  gcc
  libtool
  tree-sitter

  # service things
  polkit_gnome
  blueberry
  networkmanager_dmenu
  gammastep
  pcmanfm
  light
  freefilesync
  syncthing
  libnotify
  keepassxc
  pulseaudio
  pavucontrol
  udiskie
  # bitwarden
  # bitwarden-cli

  # qutebrowser
  unstable.qutebrowser
  # python311Packages.inotify-simple
  # python311Packages.psutil
  # python311Packages.python-daemon

  # android
  android-tools
  # unstable.scrcpy

  # latex in emacs
  # texliveMedium
  # perl5.38.2-LaTeXML
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

  # games
  # i love how you can specify retroarch cores here
  (unstable.retroarch.override {
    cores = with libretro; [
      parallel-n64
      snes9x
      swanstation
      melonds
      fbneo
    ];
  })
];

nixpkgs.config.allowUnfree = true;

services.emacs.defaultEditor = true;

# in unstable: fonts.packages = with pkgs; [
fonts.fonts = with pkgs; [
  (nerdfonts.override { fonts = [ "CodeNewRoman" "JetBrainsMono" "Ubuntu" ]; })
  noto-fonts-color-emoji
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
  stateVersion = "23.11"; # Did you read the comment?
};

}
