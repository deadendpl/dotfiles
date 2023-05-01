#!/bin/bash

echo "Enter the path to the Git repository:"
read repo_path

cd "$repo_path"
$TERM -e lazygit

