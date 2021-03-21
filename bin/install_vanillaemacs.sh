#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo "Cleaning up previously installed Emacs craps!!"
rm -rf "$HOME/.emacs"
rm -rf "$HOME/.emacs.d"
rm -rf "$HOME/.doom.d"

echo "Linking the vanilla emacs stuffs from $SCRIPTPATH"
ln -sfv "$SCRIPTPATH/emacs" "$HOME/.emacs"
ln -sfv "$SCRIPTPATH/emacs.d" "$HOME/.emacs.d"

echo "Done. Enjoy your lightweight (but not fun) emacs."
