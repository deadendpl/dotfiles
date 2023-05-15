#!/bin/bash

# this is for reading books that are put in one folder

dir="$HOME/Documents/books"
cd "$dir"

book=$(ls | rofi -config ~/.config/rofi/config-bare.rasi -dmenu -p Books)

zathura "$book"
