#!/usr/bin/env sh

cd "$HOME"

if pgrep "emacs" > /dev/null; then
  pkill emacs
fi

emacs --daemon
notify-send -i emacs "Emacs has re/started."
# emacsclient --eval '(shell-command (concat "notify-send -i emacs \"Emacs init time\" \"" (emacs-init-time) "\""))'
