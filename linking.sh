#!/bin/bash

# since I'm acively developing this I prefer to create links to files in my repo rather than copying everything over to .config 

# Set the source and target directory paths
SOURCE_DIR=~/dev/dotfiles/home/.config
TARGET_DIR=~/.config

# Loop through the files in the source directory
for filename in $SOURCE_DIR/*; do
    # Extract the basename of the file
    basename=$(basename "$filename")

    # Create a symbolic link in the target directory for the file
    ln -s "$filename" "$TARGET_DIR/$basename"
done

