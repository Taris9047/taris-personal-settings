#!/bin/bash

if [ -d "$HOME/.emacs.d" ]; then
	echo "Deleting previous emacs.d settings."
	rm -rf $HOME/.emacs
	rm -rf $HOME/.emacs.d
fi

echo "Let's install Doomemacs!!"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install


