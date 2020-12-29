#!/bin/sh

eDEXUI_DIR=$HOME/.opt/edex-ui

if [ ! -d $eDEXUI_DIR ]; then
	echo "It seems we don't have eDEX-UI on the system!"
	echo "Visit:"
	echo "https://github.com/GitSquared/edex-ui"
	echo "To install it. It might need Node.JS to be installed prior to"
	echo "be compiled."
	exit 0
else
	cd $eDEXUI_DIR && npm start
fi
