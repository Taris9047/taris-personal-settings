#!/bin/bash -e

echo ''
echo '*** Installing fonts!! ***'
echo ''

if [ ! -x "$(command -v fc-list)" ]; then
	printf 'O boy... we need fc-list!!\n'
	exit 1
fi

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo 'Installing nerd fonts! (Network)'
"$SCRIPTPATH/install_nerd_fonts.sh" > /dev/null 2>&1

if [ -z "$(fc-list | grep -i "Nanum")" ]; then
	echo 'Installing nanum (naver) fonts! (Network)'
	"$SCRIPTPATH/install_nanum_fonts.sh" > /dev/null 2>&1
fi

if [ -z "$(fc-list | grep -i "fira code")" ]; then
	echo 'Installing FiraCode! (Network)'
	"$SCRIPTPATH/install_fira_code.sh" > /dev/null 2>&1
fi

if [ -z "$(fc-list | grep -i "fira sans")" ]; then
	echo 'Installing FiraSans! (Network)'
	"$SCRIPTPATH/install_fira_sans.sh" > /dev/null 2>&1
fi

if [ -z "$(fc-list | grep -i "symbola.odf")" ]; then
	echo 'Installing Symbola (Network)'
	"$SCRIPTPATH/install_symbola_fonts.sh" > /dev/null 2>&1
fi

echo 'Font installation complete!'
echo 'Have a nice day!!'
