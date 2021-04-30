#!/bin/bash -e

echo ''
echo '*** Installing fonts!! ***'
echo ''

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

echo 'Installing nerd fonts! (Network)'
"$SCRIPTPATH/install_nerd_fonts.sh" > /dev/null 2>&1

echo 'Installing nanum (naver) fonts! (Network)'
"$SCRIPTPATH/install_nanum_fonts.sh" > /dev/null 2>&1

echo 'Installing FiraCode! (Network)'
"$SCRIPTPATH/install_fira_code.sh" > /dev/null 2>&1

echo 'Installing Symbola (Network)'
"$SCRIPTPATH/install_symbola_fonts.sh" > /dev/null 2>&1

echo 'Font installation complete!'
echo 'Have a nice day!!'
