#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo "Cleaning up previously installed Emacs craps!!"
rm -rf "$HOME/.emacs" "$HOME/.emacs.d" "$HOME/.doom.d" "$HOME/.spacemacs"

echo "Linking the vanilla emacs stuffs from $SCRIPTPATH"
ln -sfv "$SCRIPTPATH/../dotfiles/emacs" "$HOME/.emacs"
ln -sfv "$SCRIPTPATH/../dotfiles/emacs.d" "$HOME/.emacs.d"

echo "Done. Enjoy your lightweight (but not so fun) emacs."
