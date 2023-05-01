#!/bin/bash

# i'm actively developing this si I prefer to create links to my files instaed of copying them over

# Define the directory containing the files to symlink
SOURCE_DIR="./"

# Define the target directory to create symlinks in
TARGET_DIR="$HOME/.config"

# Move to the source directory
cd "$SOURCE_DIR"

# Loop through each file in the source directory
for FILE in *; do
  # Skip the script itself
  if [[ "$FILE" == "linking.sh" ]]; then
    continue
  fi

  # Create a symlink in the target directory
  ln -sfn "$(realpath "$FILE")" "$TARGET_DIR/$FILE"
done
