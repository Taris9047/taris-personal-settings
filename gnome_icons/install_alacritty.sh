#!/bin/bash

# Script directory...
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

echo "Installing alacritty icon into home directory."

# Checking alacritty existence.
if [ ! -x "$(command -v alacritty)" ]; then
  echo "ERROR: Alacritty is not detected in the systme path."
  exit 1
fi

# Making symboilc link to account desktop directory.
if [ -d "${HOME}/.local/share/applications" ]; then
  echo "Working on alacritty.desktop file..."
  ln -sfv "${SCRIPT_DIR}/alacritty.desktop" "${HOME}/.local/share/applications/alacritty.desktop"
fi

# Also making symbolic link to account icon directory.
if [ -d "${HOME}/.local/share/icons" ]; then
  echo "Working on alacritty desktop icon"
  ln -sfv "${SCRIPT_DIR}/alacritty-term.svg" "${HOME}/.local/share/icons/alacritty-term.svg"
fi

echo "Jobs done!"
