export XDG_DATA_HOME="${HOME}"/.local/share
export XDG_CONFIG_HOME="${HOME}"/.config
export XDG_STATE_HOME="${HOME}"/.local/state
export XDG_CACHE_HOME="${HOME}"/.cache
export GTK2_RC_FILES="${XDG_CONFIG_HOME}"/gtk-2.0/gtkrc
export TEXMFVAR="${XDG_CACHE_HOME}"/texlive/texmf-var
export LSP_USE_PLISTS=true
export EDITOR="emacsclient -c -t -a ''"
export VISUAL="emacsclient -c -a ''"
export HISTFILE="${XDG_STATE_HOME}"/bash/history
export W3M_DIR="${XDG_DATA_HOME}"/w3m
export GNUPGHOME="${XDG_DATA_HOME}"/gnupg
export CARGO_HOME="${XDG_DATA_HOME}"/cargo
# export XCURSOR_PATH=/usr/share/icons:${XDG_DATA_HOME}/icons
# export XCURSOR_PATH="${XDG_DATA_HOME}/icons:${XCURSOR_PATH}"
PATH="${HOME}/.local/bin:${PATH}"
