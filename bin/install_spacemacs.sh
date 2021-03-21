#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo "Installing Spacemacs!!"

echo "Removing previous emacs!"
rm -rf $HOME/.emacs $HOME/.emacs.d $HOME/.doom.d

echo "To the infinity!! and beyond!!"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -sfv "$SCRIPTPATH/../dotfiles/spacemacs" "$HOME/.spacemacs"

echo "Spacemacs downloaded! Run emacs to make it working!"
