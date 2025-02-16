#!/usr/bin/env sh

if [ "$DESKTOP_SESSION" ]; then
  gui=1
fi

if pgrep "emacs" > /dev/null; then
  emacsclient -e '(kill-emacs nil t)'
  emacs_killed=1
  sleep 1.7
fi

# emacs --daemon
notify-send -i emacs "Emacs has re/started."
if [ "$emacs_killed" = 1 ] && [ "$gui" = 1 ]; then
  emacsclient -c
fi
