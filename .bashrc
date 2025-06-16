# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source "${HOME}"/.profile

# SHELLS_CONFIG_DIR="${XDG_CONFIG_HOME}/shells"

source "${SHELLS_CONFIG_DIR}/default.sh"

# other
if test -n "$TERMUX_VERSION"; then
  # termux config
  source "${XDG_CONFIG_HOME}/shells/termux"
else
  source "${SHELLS_CONFIG_DIR}/desktop.sh"
  # apt
  if command -v apt > /dev/null; then
    source "${SHELLS_CONFIG_DIR}/apt.sh"
  fi
fi

# pacman
if command -v pacman > /dev/null; then
  source "${SHELLS_CONFIG_DIR}/pacman.sh"
fi

# nix
if command -v nixos-rebuild > /dev/null; then
  source "${SHELLS_CONFIG_DIR}/nix.sh"
fi


if [ -n "$PS1" ] && command -v starship > /dev/null; then
  eval "$(starship init bash)"
fi
