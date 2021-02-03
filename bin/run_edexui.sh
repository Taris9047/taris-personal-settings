#!/bin/sh

eDEXUI_DIR=$HOME/.opt/edex-ui

if [ ! -d $eDEXUI_DIR ]; then
	echo "It seems we don't have eDEX-UI on the system!"
	echo "Let's install it!"
	mkdir -p $HOME/.opt
	git clone https://github.com/GitSquared/edex-ui.git $eDEXUI_DIR
	cd $eDEXUI_DIR && npm run install-linux
	exit 0
fi

if [ -d $eDEXUI_DIR ]; then
	cd $eDEXUI_DIR && npm start
fi
