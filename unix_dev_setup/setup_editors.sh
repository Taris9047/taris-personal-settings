#!/bin/bash

# Written for Ubuntu... Skip this crap if your disto is not Ubuntu based.

if [ -x "$(command -v lsb_release)" ]; then
  DISTRO="$(lsb_release -is)"
  if [ -z $DISTRO ]; then
    echo "Cannot determine distro. of current OS."
    echo "Exiting..."
    exit 0
  fi
else
  IN=$(grep '^NAME' /etc/os-release)
  arrIN=(${IN//=/ })
  DISTRO=${arrIN[1]}
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

# Install scripts
#
# Sublme Text
#
install_subl_ubuntu ()
{
  echo '>>> Installing Sublme Text ... '
  wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
  echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
  sudo apt-get update && sudo apt-get -y install sublime-text
}
install_subl_fedora ()
{
  echo '>>> Installing Sublime Text ... '
  sudo rpm -v --import https://download.sublimetext.com/sublimehq-rpm-pub.gpg
  sudo dnf config-manager --add-repo https://download.sublimetext.com/rpm/stable/x86_64/sublime-text.repo
  sudo dnf install -y sublime-text
}

# Atom
install_atom_ubuntu ()
{
  echo '>>> Installing Atom'
  sudo apt-get update && sudo apt-get install -y software-properties-common apt-transport-https wget
  wget -q https://packagecloud.io/AtomEditor/atom/gpgkey -O- | sudo apt-key add -
  sudo add-apt-repository "deb [arch=amd64] https://packagecloud.io/AtomEditor/atom/any/ any main"
  sudo apt-get update && sudo apt-get -y install atom
}
install_atom_fedora ()
{
  echo '>>> Installing Atom'
  sudo dnf install -y $(curl -sL "https://api.github.com/repos/atom/atom/releases/latest" | grep "https.*atom.x86_64.rpm" | cut -d '"' -f 4)
  ATOM_INSTALLED_VERSION=$(rpm -qi atom | grep "Version" |  cut -d ':' -f 2 | cut -d ' ' -f 2)
  ATOM_LATEST_VERSION=$(curl -sL "https://api.github.com/repos/atom/atom/releases/latest" | grep -E "https.*atom-amd64.tar.gz" | cut -d '"' -f 4 | cut -d '/' -f 8 | sed 's/v//g')

  if [[ $ATOM_INSTALLED_VERSION < $ATOM_LATEST_VERSION ]]; then
    sudo dnf install -y https://github.com/atom/atom/releases/download/v${ATOM_LATEST_VERSION}/atom.x86_64.rpm
  fi
}




# Let's really install them!
if [[ "${MODE}" == "Fedora" ]]; then
  install_subl_fedora
  install_atom_fedora
elif [[ "${MODE}" == "Ubuntu" ]]; then
  install_subl_ubuntu
  install_atom_ubuntu
fi




