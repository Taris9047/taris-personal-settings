#!/usr/bin/env bash

echo "Installing Spacemacs!!"

echo "Removing previous emacs!"
rm -rf $HOME/.emacs $HOME/.emacs.d $HOME/.doom.d

echo "To the infinity!! and beyond!!"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

echo "Spacemacs downloaded! Run emacs to make it working!"
