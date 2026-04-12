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

generate_prompt() {
  local black=$(tput setaf 0)
  local red=$(tput setaf 1)
  local green=$(tput setaf 2)
  local light_green=$(sed -n '11{p;q}' "${XDG_CACHE_HOME}/wal/colors")
  local light_green_r=$((16#${light_green:1:2}))
  local light_green_g=$((16#${light_green:3:2}))
  local light_green_b=$((16#${light_green:5:2}))
  light_green="\033[38;2;${light_green_r};${light_green_g};${light_green_b}m"
  local light_green_end="\033[0m"
  local light_magenta=$(sed -n '14{p;q}' "${XDG_CACHE_HOME}/wal/colors")
  local light_magenta_r=$((16#${light_magenta:1:2}))
  local light_magenta_g=$((16#${light_magenta:3:2}))
  local light_magenta_b=$((16#${light_magenta:5:2}))
  light_magenta="\033[38;2;${light_magenta_r};${light_magenta_g};${light_magenta_b}m"
  local light_magenta_end="\033[0m"
  local yellow=$(tput setaf 3)
  local blue=$(tput setaf 4)
  local magenta=$(tput setaf 5)
  local cyan=$(tput setaf 6)
  local reset=$(tput sgr0)

  local git_branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
  local user="${light_green}$(whoami)${light_green_end}"
  local dir="\w"
  local dir="${light_green}${dir}${light_green_end}"
  local arrow="${light_green}❯${light_green_end}"
  local hostname="$(hostname)"

  if [[ -n $git_branch ]]; then
    git_branch="${light_magenta}(${git_branch})${light_magenta_end}"
    PS1="\n${user}@${hostname} ${dir} ${git_branch}\n${arrow} "
  else
    PS1="\n${user}@${hostname} ${dir}\n${arrow} "
  fi
}

PROMPT_COMMAND=generate_prompt

# if [ -n "$PS1" ] && command -v starship > /dev/null; then
#   eval "$(starship init bash)"
# fi
