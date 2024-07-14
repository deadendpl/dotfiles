#!/usr/bin/env sh

cd "$HOME"

if pgrep "emacs" > /dev/null; then
  pkill emacs
  emacs_killed=1
fi

emacs --daemon
notify-send -i emacs "Emacs has re/started."
if [ $emacs_killed == 1 ]; then
  emacsclient -c
fi
# emacsclient --eval '(shell-command (concat "notify-send -i emacs \"Emacs init time\" \"" (emacs-init-time) "\""))'
