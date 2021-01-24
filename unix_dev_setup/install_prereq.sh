#!/bin/bash -e

#
# TODO: Need to populate proper package list for Fedora and Arch
#

DISTRO="$(lsb_release -is)"
if [ -z $DISTRO ]; then
  echo "Cannot determine distro. of current OS."
  echo "Exiting..."
  exit 0
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

echo "Current linux distribution seems $MODE based one."

Ubuntu_packages=( \
  "build-essential"  \
  "flex" \
  "bison" \
  "zlib1g" \
  "zlib1g-dev" \
  "openssl" \
  "libssl-dev" \
  "libsqlite3-dev" \
  "libncursesw5-dev" \
  "libreadline-dev" \
  "libssl-dev" \
  "libgdbm-dev" \
  "libc6-dev" \
  "libsqlite3-dev" \
  "tk-dev" \
  "libbz2-dev" \
  "libicu-dev" \
  "libffi-dev" \
  "autotools-dev" \
  "python3-dev" \
  "libncurses5-dev" \
  "libxml2-dev" \
  "libedit-dev" \
  "swig" \
  "doxygen" \
  "graphviz" \
  "xz-utils" \
  "ruby" \
  "ruby-dev" \
  "git-lfs" \
  "tree" \
  "libzmq3-dev" \
  "libtool-bin" \
  "dos2unix" \
  "liblzma-dev" \
  "lzma" \
  "pkg-config" \
  "libbz2-dev" \
  "libncurses5-dev" \
  "libexpat1-dev" \
  "libgdbm-dev" \
  "tk-dev" \
  "libgc-dev" \
  "python-cffi" \
  "libopenblas-dev" \
  "valgrind" \
  "cmake" \
  "cmake-gui" \
  "ninja-build" \
  "autoconf" \
  "automake" \
  "vim" \
  "emacs" \
  "ttf-bitstream-vera" \
  "subversion" \
  "git" \
  "wget" \
  "curl" )

Fedora_packages=( \
  "ruby" \
  "ruby-devel" \
  "cmake" \
  "tk-devel" \
  "bzip2-libs" \
  "emacs" \
  "subversion" \
  "git" \
  "wget" \
  "curl" \
  "valgrind" \
  )

Arch_packages=( \
  "base-devel" \
  "git" \
  "wget" \
  "curl" \
  "vim" \
  "neovim" \
  "emacs" \
  "valgrind" \
  )


Ruby_gems=( \
  "rsense" \
  "open3" \
  "json" \
  "hjson" )

array_to_string ()
{
  arr=("$@")
  echo ${arr[*]}
}

install_prereq_Ubuntu ()
{
  pkgs=$( array_to_string "${Ubuntu_packages[@]}")
  sudo apt-get -y update && sudo apt-get -y install $pkgs
}

install_prereq_Fedora ()
{
  pkgs=$( array_to_string "${Fedora_packages[@]}" )
  sudo dnf groupinstall "Development Tools" "Development Libraries"
  sudo dnf -y update && sudo dnf -y install $pkgs
}

install_prereq_Arch ()
{
  pkgs=$( array_to_string "${Arch_packages[@]}" )
  sudo pacman -Syyu $pkgs
}


if [[ "$MODE" == "Ubuntu" ]]; then
  install_prereq_Ubuntu
elif [[ "$MODE" == "Fedora" ]]; then
  install_prereq_Fedora
elif [[ "$MODE" == "Arch" ]]; then
  install_prereq_Arch
fi 