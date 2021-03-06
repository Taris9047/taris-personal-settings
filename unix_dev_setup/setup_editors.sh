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
  DISTRO=$(echo $IN | sed -E 's/\"//g' | sed -E 's/NAME=//')
fi

echo "Detecting distro..."
echo "... Looks like your distro is: $DISTRO"

# Some Distro information
# Some Distro information
Debian_base=("Debian GNU/Linux")
Ubuntu_base=("Ubuntu" "Linuxmint" "Pop" "Pop\!_OS")
Fedora_base=("Fedora" "CentOS Linux" "CentOS Stream" "Red Hat Enterprise Linux")
openSUSE_base=("openSUSE" "openSUSE Leap")
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
  if [ ! -x "$(command -v subl)" ]; then
    echo '>>> Installing Sublme Text ... '
    wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
    echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
    sudo apt-get -y update && sudo apt-get -y install sublime-text
  else
    echo '>>> Updating Sublime Text ... '
    sudo apt-get -y update && sudo apt-get -y upgrade
  fi
}
install_subl_fedora ()
{
  if [ ! -x "$(command -v subl)" ]; then
    echo '>>> Installing Sublime Text ... '
    sudo rpm -v --import https://download.sublimetext.com/sublimehq-rpm-pub.gpg
    sudo dnf -y config-manager --add-repo https://download.sublimetext.com/rpm/stable/x86_64/sublime-text.repo
    sudo dnf install -y sublime-text
  else
    echo '>>> Updating Sublime Text ... '
    sudo dnf -y update
  fi
}

# Atom
install_atom_ubuntu ()
{
  if [ ! -x "$(command -v atom)" ]; then
    echo '>>> Installing Atom ... '
    sudo apt-get update && sudo apt-get install -y software-properties-common apt-transport-https wget
    wget -q https://packagecloud.io/AtomEditor/atom/gpgkey -O- | sudo apt-key add -
    sudo add-apt-repository "deb [arch=amd64] https://packagecloud.io/AtomEditor/atom/any/ any main"
    sudo apt-get -y update && sudo apt-get -y install atom
  else
    echo '>>> Updating Atom ... '
    sudo apt-get -y update && sudo apt-get -y install atom
  fi
}
install_atom_fedora ()
{
  echo '>>> Installing and Updating Atom ... '
  sudo dnf install -y $(curl -sL "https://api.github.com/repos/atom/atom/releases/latest" | grep "https.*atom.x86_64.rpm" | cut -d '"' -f 4)
  ATOM_INSTALLED_VERSION=$(rpm -qi atom | grep "Version" |  cut -d ':' -f 2 | cut -d ' ' -f 2)
  ATOM_LATEST_VERSION=$(curl -sL "https://api.github.com/repos/atom/atom/releases/latest" | grep -E "https.*atom-amd64.tar.gz" | cut -d '"' -f 4 | cut -d '/' -f 8 | sed 's/v//g')

  if [[ $ATOM_INSTALLED_VERSION < $ATOM_LATEST_VERSION ]]; then
    sudo dnf install -y https://github.com/atom/atom/releases/download/v${ATOM_LATEST_VERSION}/atom.x86_64.rpm
  fi
}

# VS Code --> who knows, this will replace Atom in near future...
install_vscode_ubuntu ()
{
  if [ ! -x "$(command -v code)" ]; then
    echo '>>> Installing VSCode ... '
    wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
    sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
    sudo sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list'
    sudo apt-get -y install apt-transport-https
    sudo apt-get -y update
    sudo apt-get -y install code # or code-insiders
		rm -rf ./packages.microsoft.gpg
	else
    echo '>>> Updating VSCode ... '
    sudo apt-get -y update && sudo apt-get -y upgrade
  fi
}

install_vscode_fedora ()
{
  if [ ! -x "$(command -v code)" ]; then
    echo '>>> Installing VSCode ... '
    sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
    sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
    sudo dnf -y check-update
    sudo dnf -y install code
  else
    echo '>>> Updating VSCode ... '
    sudo dnf -y update
  fi
}


# Let's really install them!
case ${MODE} in
  "Fedora")
    install_subl_fedora
    install_atom_fedora
    install_vscode_fedora
    ;;
  "Ubuntu")
    install_subl_ubuntu
    install_atom_ubuntu
    install_vscode_ubuntu
    ;;
esac
