#!/bin/bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if [ -d "$HOME/.emacs.d" ] || [ -f "$HOME/.emacs" ]; then
	echo "Deleting previous emacs settings."
	rm -rf $HOME/.emacs
	rm -rf $HOME/.emacs.d
fi

echo "Let's install Doomemacs!!"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom -y install

echo "Ok, migrating the settings file!!"
# cp -vfr $SCRIPTPATH/../dotfiles/doom.d/*.el $HOME/.doom.d/
ln -sfv "${SCRIPTPATH}/../dotfiles/doom.d/init.el" "${HOME}/.doom.d/init.el"
ln -sfv "${SCRIPTPATH}/../dotfiles/doom.d/config.el" "${HOME}/.doom.d/config.el"
ln -sfv "${SCRIPTPATH}/../dotfiles/doom.d/packages.el" "${HOME}/.doom.d/packages.el"

$HOME/.emacs.d/bin/doom -y sync

echo "Have fun!!"
