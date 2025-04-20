#!/usr/bin/env sh

if [ "$DESKTOP_SESSION" ]; then
  gui=1
fi

if pidof emacs > /dev/null; then
  pkill emacs
  emacs_killed=1
fi

if [ "$emacs_killed" = 1 ]; then
  while pidof emacs > /dev/null; do
    sleep 0.05
  done
fi

emacs --daemon
if [ "$emacs_killed" = 1 ] && [ "$gui" = 1 ]; then
  emacsclient -c &
fi
notify-send -i emacs "Emacs has re/started."
