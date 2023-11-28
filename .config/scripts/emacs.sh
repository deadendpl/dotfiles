#!/usr/bin/env sh

while pgrep "emacs"
do
  pgrep "emacs" | xargs kill
  echo "Emacs launches..."
  emacs --daemon
  notify-send -i emacs "Emacs has started."
  exit
done

echo "Emacs launches..."
emacs --daemon
notify-send -i emacs "Emacs has started."
