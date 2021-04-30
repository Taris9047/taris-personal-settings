#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo "Installing Spacemacs!!"

#echo "Removing previous emacs!"
#rm -rf $HOME/.emacs $HOME/.emacs.d $HOME/.doom.d

# Backing up previous emacs installation
if [ -d "$HOME/.emacs.d" ]; then
  rm -rf "$HOME/.emacs.d.bak" "$HOME/.emacs"
  mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak"
fi

echo "To the infinity!! and beyond!!"
git clone https://github.com/syl20bnr/spacemacs "$HOME/.emacs.d"
[ -f "$HOME/.spacemacs" ] && rm -rf "$HOME/.spacemacs"
ln -sfv "$SCRIPTPATH/../dotfiles/spacemacs" "$HOME/.spacemacs"

echo "Spacemacs downloaded! Run emacs to make it working!"
