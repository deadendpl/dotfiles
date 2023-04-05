#!/bin/sh

bookmark="$(xclip -o)"
file="$HOME/Documents/bookmarks"

if grep -q "^$bookmark$" "$file"; then
	notify-send "Oopsie." "Already bookmarked!"
else
	notify-send "Bookmark added!" "$bookmark is now saved to the bookmarks."
	echo "$bookmark" >> "$file"
fi
