#!/bin/bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if [ -d "$HOME/.emacs.d" ]; then
	echo "Deleting previous emacs.d settings."
	rm -rf $HOME/.emacs
	rm -rf $HOME/.emacs.d
fi

echo "Let's install Doomemacs!!"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

echo "Ok, migrating the settings file!!"
cp -vfr $SCRIPTPATH/../dotfiles/doom.d/*.el $HOME/doom.d/
$HOME/.emacs/bin/doom sync

echo "Have fun!!"
