# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source "${HOME}"/.profile

SHELLS_CONFIG_DIR="${XDG_CONFIG_HOME}/shells"

source "${SHELLS_CONFIG_DIR}/default.sh"

# apt
if which apt > /dev/null 2>&1; then
  source "${SHELLS_CONFIG_DIR}/apt.sh"
fi

# pacman
if which pacman > /dev/null 2>&1; then
  source "${SHELLS_CONFIG_DIR}/pacman.sh"
fi

# nix
if which nixos-rebuild > /dev/null 2>&1; then
  source "${SHELLS_CONFIG_DIR}/nix.sh"
fi

# other
if test -n "$TERMUX_VERSION"; then
  # termux config
  source "${XDG_CONFIG_HOME}/shells/termux"
else
  source "${SHELLS_CONFIG_DIR}/desktop.sh"
fi


if [ -n "$PS1" ]; then
  eval "$(starship init bash)"
fi
