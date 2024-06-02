# emacs keybindings
fish_default_key_bindings

fenv source "$HOME"/.profile

set SHELLS_CONFIG_DIR "$XDG_CONFIG_HOME/shells"

source "$SHELLS_CONFIG_DIR/default.sh"

# nix
if which nixos-rebuild > /dev/null 2>&1
  source "$SHELLS_CONFIG_DIR/nix.sh"
end

if status is-interactive
  set fish_greeting
end

set EDITOR emacsclient -t -a ""
set VISUAL emacsclient -c -a ""

# apt
if which apt > /dev/null 2>&1
  source "$SHELLS_CONFIG_DIR/apt.sh"
end

# pacman and yay on Arch
if which pacman > /dev/null 2>&1
  source "$SHELLS_CONFIG_DIR/pacman.sh"
end

if test -n "$TERMUX_VERSION"
  # termux config
  source "$SHELLS_CONFIG_DIR/termux"
else
  starship init fish | source
  source "$SHELLS_CONFIG_DIR/desktop.sh"
  # going to last directory from lf
  function lfcd
    set tmp (mktemp)
    # `command` is needed in case `lfcd` is abbred to `lf`
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
end
