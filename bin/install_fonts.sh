#!/bin/bash -e

echo ''
echo '*** Installing fonts!! ***'
echo ''

echo 'Installing nerd fonts! (Network)'
./install_nerd_fonts.sh 2> /dev/null

echo 'Installing nanum (naver) fonts! (Network)'
./install_nanum_fonts.sh 2> /dev/null

echo 'Font installation complete!'
echo 'Have a nice day!!'
