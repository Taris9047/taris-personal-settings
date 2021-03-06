#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo "Cleaning up previously installed Emacs craps!!"
rm -rf "$HOME/.emacs" "$HOME/.emacs.d" 
# rm -rf "$HOME/.doom.d" "$HOME/.spacemacs"

echo "Linking the vanilla emacs stuffs from $SCRIPTPATH"
#ln -sfv "$SCRIPTPATH/../dotfiles/emacs" "$HOME/.emacs"
#ln -sfv "$SCRIPTPATH/../dotfiles/emacs.d" "$HOME/.emacs.d"
EMCONF_DIR="$SCRIPTPATH/../dotfiles/emacs.d"
EMACSD="$HOME/.emacs.d"
mkdir "$EMACSD"
ln -sfv "$EMCONF_DIR/config.org" "$EMACSD/config.org"
ln -sfv "$EMCONF_DIR/init.el" "$EMACSD/init.el"
ln -sfv "$EMCONF_DIR/bokma-emacs.org" "$EMACSD/emacs-logo.png"

echo "Done. Enjoy your lightweight (but not so fun) emacs."
