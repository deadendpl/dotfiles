#!/usr/bin/env bash

org_text=$(cat $HOME/org-roam/web_bookmarks.org)

# Extract link names and URLs using a regex pattern
links=$(echo "$org_text" | grep -oE '\[\[[^]]+\]\[[^]]+\]\]')

# Extract and format link names and URLs
link_names=()
link_urls=()
while IFS= read -r link; do
    link_name=$(echo "$link" | sed -E 's/\[\[.*\]\[([^]]+)\]\]/\1/')
    link_url=$(echo "$link" | sed -E 's/\[\[([^]]+)\]\[.*\]\]/\1/')
    link_names+=("$link_name")
    link_urls+=("$link_url")
done <<< "$links"

# Prompt the user to either select an existing link or enter a custom URL
options=("$(printf "%s\n" "${link_names[@]}")" "Custom URL")
selected_option=$(printf "%s\n" "${options[@]}" | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -i -p "Select a link:")

if [ "$selected_option" = "Custom URL" ]; then
    # Ask the user for a custom URL
    custom_url=$(rofi -config ~/.config/rofi/config-bare.rasi -dmenu -p "Enter a URL:")

    # Add "http://" prefix if it's missing
    if ! [[ "$custom_url" =~ ^https?:// ]]; then
        custom_url="http://$custom_url"
    fi

    xdg-open "$custom_url"
else
    # Find the selected index based on the selected option
    selected_index=-1
    for index in "${!options[@]}"; do
        if [ "${options[$index]}" = "$selected_option" ]; then
            selected_index=$index
            break
        fi
    done

    # Open the selected link using xdg-open
    if [ "$selected_index" -ge 0 ]; then
        selected_url="${link_urls[$selected_index]}"
        xdg-open "$selected_url"
    fi
fi
