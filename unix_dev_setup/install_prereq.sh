#!/bin/bash -e

#
# TODO: Need to populate proper package list for Fedora and Arch
#

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
  "screen" \
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
  "libnuma-dev" \
  "python-cffi" \
  "libopenblas-dev" \
  "libx11-dev" \
  "libxpm-dev" \
  "libxft-dev" \
  "libxext-dev" \
  "libpng-dev" \
  "libjpeg-dev" \
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
  "curl" \
  "neofetch" \
  )

Fedora_packages=( \
  "ruby" \
  "ruby-devel" \
  "cmake" \
  "cmake-gui" \
  "libuuid" \
  "libuuid-devel" \
  "tk-devel" \
  "bzip2-libs" \
  "libffi-devel" \
  "numactl-devel" \
  "xz-devel" \
  "expat-devel" \
  "openblas" \
  "openblas-devel" \
  "blas" \
  "blas-devel" \
  "lapack" \
  "sqlite" \
  "sqlite-devel" \
  "sqlite-tcl" \
  "zlib" \
  "zlib-devel" \
  "binutils" \
  "libX11-devel" \
  "libXpm-devel" \
  "libXft-devel" \
  "libXext-devel" \
  "libXt-devel" \
  "openssl" \
  "openssl-devel" \
  "redhat-lsb-core" \
  "gcc-gfortran" \
  "pcre-devel" \
  "mesa-libGL-devel" \
  "mesa-libGLU-devel" \
  "glew-devel" \
  "ftgl-devel" \
  "mysql-devel" \
  "fftw-devel" \
  "cfitsio-devel" \
  "graphviz-devel" \
  "avahi-compat-libdns_sd-devel" \
  "openldap-devel" \
  "libxml2-devel" \
  "gsl-devel" \
  "emacs" \
  "subversion" \
  "git" \
  "git-lfs" \
  "doxygen" \
  "wget" \
  "curl" \
  "valgrind" \
  "valgrind-devel" \
  "vim" \
  "neovim" \
  "screen" \
  "ninja-build" \
  "neofetch" \
  )

Arch_packages=( \
  "base-devel" \
  "flex" \
  "bison" \
  "git" \
  "wget" \
  "curl" \
  "vim" \
  "neovim" \
  "emacs" \
  "valgrind" \
  "swig" \
  "cmake" \
  "pkgconf" \
  "ruby" \
  "ruby-irb" \
  "openblas" \
  "lapack" "cblas" \
  "mercurial" \
  "subversion" \
  "neofetch" \
  )


Ruby_gems=( \
  "rsense" \
  "open3" \
  "json" )

array_to_string ()
{
  arr=("$@")
  echo ${arr[*]}
}

install_prereq_Ubuntu ()
{
  pkgs=$( array_to_string "${Ubuntu_packages[@]}")
  gems=$( array_to_string "${Ruby_gems[@]}")
  sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y install $pkgs
  sudo gem install $gems
}

install_prereq_Fedora ()
{
  pkgs=$( array_to_string "${Fedora_packages[@]}" )
  gems=$( array_to_string "${Ruby_gems[@]}")
  sudo dnf groupinstall "Development Tools" "Development Libraries"
  sudo dnf -y update && sudo dnf -y upgrade && sudo dnf -y install $pkgs
  sudo gem install $gems
}

install_prereq_Arch ()
{
  pkgs=$( array_to_string "${Arch_packages[@]}" )
  gems=$( array_to_string "${Ruby_gems[@]}")
  sudo pacman -Syyu $pkgs
  sudo gem install $gems
}


if [[ "$MODE" == "Ubuntu" ]]; then
  install_prereq_Ubuntu
elif [[ "$MODE" == "Fedora" ]]; then
  install_prereq_Fedora
elif [[ "$MODE" == "Arch" ]]; then
  install_prereq_Arch
fi

# Putting up some system info!
if [ -x "$(command -v neofetch)" ]; then
  neofetch
fi

echo ""
echo "=========================================="
echo "| Prereq. package installation finished! |"
echo "|                                        |"
echo "| Now we can run ./unix_dev_setup.rb     |"
echo "|                                        |"
echo "| Hopefully, it compiles everything fine.|"
echo "=========================================="
echo ""

if [[ "$MODE" == "Arch" ]]; then
  echo "=========================================="
  echo "| Note on Arch based Linux!!             |"
  echo "|                                        |"
  echo "| Arch based distros do not usually      |"
  echo "| provide -dev packages.                 |"
  echo "| Thus, we may need to rather use ABS or |"
  echo "| just use package version rather than   |"
  echo "| compiling everything!!                 |"
  echo "=========================================="
  echo ""
  echo ">> Cheap way to finish all the installation in Arch based distros:"
  echo "sudo pacman -Syyu ruby lua python python-pip python2 python2-pip pypy3 clang nodejs npm boost cmake tk sqlite"
fi
