#!/bin/sh

DISTRO="$(lsb_release -is)"
if [ -z $DISTRO ]; then
  DISTRO='unknown'
fi

# Some Distro information
Ubuntu_base=("Ubuntu" "Linuxmint")
Fedora_base=("Fedora" "openSUSE project")
Arch_base=("Arch" "Manjaro")

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

