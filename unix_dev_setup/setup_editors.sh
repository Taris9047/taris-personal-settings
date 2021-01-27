#!/bin/bash

# Written for Ubuntu... Skip this crap if your disto is not Ubuntu based.

DISTRO="$(lsb_release -is)"
if [ -z $DISTRO ]; then
  echo "Cannot determine distro. of current OS."
  echo "Exiting..."
  exit 0
fi

# Some Distro information
Ubuntu_base=("Ubuntu" "Linuxmint")
Fedora_base=("Fedora" "openSUSE project")
Arch_base=("ArchLinux" "ManjaroLinux")

# Supported modes
# "Ubuntu" "Fedora" "Arch"
MODE=''

if [[ " ${Ubuntu_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Ubuntu"
elif [[ " ${Fedora_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Fedora"
elif [[ " ${Arch_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Arch"
fi

echo "Current linux distribution seems $MODE based one."

if [[ "${MODE}" != "Ubuntu" ]]; then
	echo "It seems the distro is not Ubuntu based..."
	echo "*** I'm not sure this script would work! ***"
	echo "Exiting"
	edit(-1)
fi


# Sublime Text
echo '>>> Installing Sublme Text ... '
wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
sudo apt-get update && sudo apt-get -y install sublime-text

# Atom
echo '>>> Installing Atom'
sudo apt-get update && sudo apt-get install -y software-properties-common apt-transport-https wget
wget -q https://packagecloud.io/AtomEditor/atom/gpgkey -O- | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://packagecloud.io/AtomEditor/atom/any/ any main"
sudo apt-get update && sudo apt-get -y install atom


