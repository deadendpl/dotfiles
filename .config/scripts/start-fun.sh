#!/bin/bash

dir="/home/oliwier/dev/dotfiles/.config/startup"

# Get a randomized list of all files in the directory
file_list=($(ls "$dir" | shuf))

# Select the first file in the randomized list
selected_file="${file_list[0]}"

# Execute the selected file using bash
bash "$dir/$selected_file"

