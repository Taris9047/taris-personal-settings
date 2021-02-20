#!/bin/bash

GOOGLE_DRIVE_LOCAL=$HOME/.google-drive
FONTS_DIR=$GOOGLE_DRIVE_LOCAL/Fonts
TMP_DIR='./tmp_dir'
DEST_FONTS_DIR="$HOME/.fonts"

if [ ! -x $(command -v rclone) ]; then
  echo "Install rclone and set up Google Drive!"
  echo "Default google drive path is: $GOOGLE_DRIVE_LOCAL"
  exit 1
fi

if [ ! -x $(command -v 7z) ]; then
  echo "7z cannot be found! We must have it installed on the system!!"
  exit 1
fi

if [ ! -d $DEST_FONTS_DIR ]; then
  mkdir -pv $DEST_FONTS_DIR
fi

if [ ! -d $TMP_DIR ]; then
  mkdir -pv $TMP_DIR
fi

echo ""
echo "Downloading Fonts from GoogleDrive!"
rsync -azvh --force --info=progress2 $FONTS_DIR/NerdFonts.7z $TMP_DIR/
echo ""
echo "Extracting!"
7z x $TMP_DIR/NerdFonts.7z -o$TMP_DIR/
echo ""
echo "Installing Fonts to home directory!"
rsync -azvh --force --info=progress2 $TMP_DIR/NerdFontLinux/* $DEST_FONTS_DIR/
fc-cache -fv

echo "All done.. Cleaning up!"
rm -rf $TMP_DIR
