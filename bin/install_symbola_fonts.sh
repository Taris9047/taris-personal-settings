#!/usr/bin/env bash

CWD=$(pwd -P)

tmp_dir="/tmp/.symbola_tmp"
dest_dir="$HOME/.local/share/fonts/SymbolaFonts"
font_url="https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip"

if [ ! -d "$tmp_dir" ]; then
    mkdir -p "$tmp_dir"
fi

if [ ! -d "$dest_dir" ]; then
    mkdir -p "$dest_dir"
fi

echo "Downloading and Installing Symbola Fonts"
cd "$tmp_dir" && wget "$font_url" > /dev/null 2>&1
cd "$tmp_dir" && unzip ./Symbola.zip > /dev/null 2>&1
cp -vfr "$tmp_dir/Symbola.otf" "$dest_dir/" > /dev/null 2>&1
rm -rf "$tmp_dir"

echo "Updating fonts cache"
fc-cache -vf > /dev/null 2>&1


