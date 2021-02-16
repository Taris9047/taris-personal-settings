#!/bin/bash -e

#
# TODO: Need to populate proper package list for other distros.
# There are tons of strange distros!!
#

CWD=`pwd -P`
TMP_PKG_DIR=$CWD/pkg_dir

if [ -x "$(command -v lsb_release)" ]; then
  DISTRO="$(lsb_release -is)"
  if [ -z $DISTRO ]; then
    echo "Cannot determine distro. of current OS."
    echo "Exiting..."
    exit 0
  fi
else
  IN=$(grep '^NAME' /etc/os-release)
  DISTRO=$(echo $IN | tr -d "\"" | tr -d "NAME=")
fi

# Update this line for RHEL.. later..
if [ "$DISTRO" == "CentOS Linux" ]; then
  mkdir -pv $TMP_PKG_DIR
fi


# Some Distro information
Debian_base=("Debian GNU/Linux")
Ubuntu_base=("Ubuntu" "Linuxmint")
Fedora_base=("Fedora" "CentOS Linux")
Arch_base=("ArchLinux" "ManjaroLinux")

# Supported modes
# "Ubuntu" "Fedora" "Arch"
MODE=''

if [[ " ${Ubuntu_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Ubuntu"
elif [[ " ${Debian_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Debian"
elif [[ " ${Fedora_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Fedora"
elif [[ " ${Arch_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="Arch"
fi

echo "Current linux distribution seems $MODE based one."

Debian_packages=(
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
  "wget" \
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
  "blas" \
  "lapack" \
  "sqlite" \
  "sqlite-devel" \
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
  "ftgl-devel" \
  "fftw-devel" \
  "cfitsio-devel" \
  "openldap-devel" \
  "libxml2-devel" \
  "gsl-devel" \
  "emacs" \
  "subversion" \
  "git" \
  "git-lfs" \
  "wget" \
  "curl" \
  "valgrind" \
  "valgrind-devel" \
  "vim" \
  "neovim" \
  "screen" \
  "neofetch" \
  )

# Well, apparently, there were some package differences 
# between Fedora and CentOS/RHEL...
Fedora_additional_packages=( \
  "openblas-devel" \
  "blas-devel" \
  "mysql-devel" \
  "glew-devel" \
  "graphviz-devel" \
  "avahi-compat-libdns_sd-devel" \
  "doxygen" \
  "ninja-build" \
  )
# Many of them aren't really 
RHEL_additional_packages=( \
  "openblas-devel" \
  "blas-devel" \
  "glew-devel" \
  "graphviz-devel" \
  "avahi-compat-libdns_sd-devel" \
  "doxygen" \
  "ninja-build" \
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
  sudo apt-get -y update && sudo apt-get -y upgrade
  sudo apt-get -y install $pkgs
  sudo /usr/bin/gem install $gems
}

install_prereq_Debian ()
{
  install_prereq_Ubuntu
#  pkgs=$( array_to_string "${Debian_packages[@]}")
#  gems=$( array_to_string "${Ruby_gems[@]}")
#  sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y install $pkgs
#  sudo /usr/bin/gem install $gems
}

install_prereq_Fedora ()
{
  pkgs=$( array_to_string "${Fedora_packages[@]}" )
  add_pkgs=()
  gems=$( array_to_string "${Ruby_gems[@]}")
  # Fedora
  if [ "$DISTRO" == "Fedora" ]; then
    sudo dnf -y groupinstall "Development Tools" "Development Libraries"
    add_pkgs=$( array_to_string "${Fedora_additional_packages[@]}" )
  # In case CentOS or RHEL
  else
    sudo dnf -y install dnf-plugins-core
    sudo dnf -y install epel-release
    sudo dnf config-manager --set-enabled powertools
    sudo dnf -y groupinstall "Development Tools" "Additional Development"
    add_pkgs=$( array_to_string "${RHEL_additional_packages[@]}" )
  fi 
  sudo dnf -y update && sudo dnf -y upgrade
  sudo dnf -y install $pkgs $add_pkgs
  sudo /usr/bin/gem install $gems
}

install_prereq_Arch ()
{
  pkgs=$( array_to_string "${Arch_packages[@]}" )
  gems=$( array_to_string "${Ruby_gems[@]}")
  sudo pacman -Syyu $pkgs
  sudo /usr/bin/gem install $gems
}


if [[ "$MODE" == "Ubuntu" ]]; then
  install_prereq_Ubuntu
elif [[ "$MODE" == "Debian" ]]; then
  install_prereq_Debian
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

# Cleanup my mess.
if [ -d $TMP_PKG_DIR ]; then
  rm -rf $TMP_PKG_DIR
fi
