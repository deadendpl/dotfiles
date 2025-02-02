#!/usr/bin/env sh

file="${HOME}/Sync/foo/wazne/bookmarks"

if sort $file -o $file >> /dev/null; then
  notify-send "Bookmarks file got sorted"
else
  notify-send "Bookmarks file didn't get sorted"
fi
