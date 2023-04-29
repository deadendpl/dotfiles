#!/bin/sh

if [ "$XDG_SESSION_TYPE" = "x11" ]; then
    clipboard_cmd="xclip -o"
else
    clipboard_cmd="wl-paste"
fi

bookmark="$($clipboard_cmd)"
file="$HOME/Documents/bookmarks"

if grep -q "^$bookmark$" "$file"; then
    notify-send "Oopsie." "Already bookmarked!"
else
    notify-send "Bookmark added!" "$bookmark is now saved to the bookmarks."
    echo "$bookmark" >> "$file"
fi
