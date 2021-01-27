#!/bin/bash

# Written for Ubuntu... Skip this crap if your disto is not Ubuntu based.

# Sublime Text
echo '>>> Installing Sublme Text ... '
wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
sudo apt-get update && sudo apt-get -y install sublime-text

# Atom
echo '>>> Installing Atom'
sudo apt install software-properties-common apt-transport-https wget
wget -q https://packagecloud.io/AtomEditor/atom/gpgkey -O- | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://packagecloud.io/AtomEditor/atom/any/ any main"
sudo apt-get update && sudo apt-get -y install atom


