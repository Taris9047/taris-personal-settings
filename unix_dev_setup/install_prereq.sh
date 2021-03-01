#!/bin/bash -e

#
# TODO: Need to populate proper package list for other distros.
# There are tons of strange distros!!
# --> Ubuntu 20.04, Debian, Fedora (F33), CentOS (8), RHEL (8), openSUSE Leap 
# are ok now.
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
  DISTRO=$(echo $IN | sed -E 's/\"//g' | sed -E 's/NAME=//')
fi

echo "Detecting distro..."
echo "... Looks like your distro is: $DISTRO"

# Some Distro information
Debian_base=("Debian GNU/Linux")
Ubuntu_base=("Ubuntu" "Linuxmint")
Fedora_base=("Fedora" "CentOS Linux" "Red Hat Enterprise Linux")
openSUSE_base=("openSUSE" "openSUSE Leap")
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
elif [[ " ${openSUSE_base[@]} " =~ " ${DISTRO} " ]]; then
  MODE="openSUSE"
fi

echo "Current linux distribution seems $MODE based one."

Debian_packages=( \
  "build-essential"  \
  "flex" \
  "bison" \
  "zlib1g" \
  "zlib1g-dev" \
  "openssl" \
  "libssl-dev" \
  "libsqlite3-dev" \
  "libncursesw5-dev" \
	"libyaml-dev" \
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

Ubuntu_packages=${Debian_packages[@]}

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

# openSUSE
openSUSE_packages=( \
  "wget" \
  "ruby" \
  "ruby-devel" \
  "gcc10" "gcc10-g++" \
  "cmake" \
  "cmake-gui" \
  "libuuid-devel" \
  "tk-devel" \
  "bzip2" \
  "libffi-devel" \
  "numactl" \
  "xz-devel" \
  "libexpat1" \
  "openblas-devel" \
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
  "pcre-devel" \
  "Mesa" \
  "ftgl-devel" \
  "fftw-devel" \
  "cfitsio-devel" \
  "yast2-auth-server" \
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
  "openblas-devel" \
  "blas-devel" \
  "mysql-devel" \
  "glew-devel" \
  "graphviz-devel" \
  "doxygen" \
	"unicode-emoji" \
  "ninja" \
)
  
# Arch
Arch_packages=( \
  "base-devel" \
  "gcc-fortran" \
  "libffi" \
  "flex" \
  "tk" \
  "bison" \
  "zlib" \
  "bzip2" \
  "cfitsio" \
  "expat" \
  "pcre" \
  "git" \
  "git-lfs" \
  "doxygen" \
  "wget" \
  "curl" \
  "vim" \
  "neovim" \
  "emacs" \
  "valgrind" \
  "swig" \
  "sqlite" \
  "mariadb" \
  "cmake" \
  "pkgconf" \
  "xz" \
  "p7zip" \
  "ruby" \
  "ruby-irb" \
  "openblas" \
  "gsl" \
  "ftgl" \
  "fftw" \
  "openssl" \
  "openldap" \
  "numactl" \
  "glew" \
  "graphviz" \
  "libxml2" \
  "libx11" \
  "libxpm" \
  "libxft" \
  "libxext" \
  "libxt" \
  "screen" \
  "lapack" \
  "mercurial" \
  "subversion" \
	"unicode-emoji" \
  "ninja" \
  "neofetch" \
  )


Ruby_gems=( \
  "rsense" \
  "open3" \
  "json" \
  "ruby-progressbar" \
  "tty-spinner" \
  )

#
# Note on open3: Ruby 2.5.. which comes with CentOS/RHEL cannot support
# newer 0.1.1 version. So, 0.1.0 will be installed on those systems.
# Let's hope they can work with my codes without too much problem.
#
Ruby_gems_RHEL=( \
  "rsense" \
  "json" \
  "ruby-progressbar" \
  "tty-spinner" \
)

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
  gems=()
  # Fedora
  if [[ "$DISTRO" == *"Fedora"* ]]; then
    sudo dnf -y groupinstall "Development Tools" "Development Libraries"
    add_pkgs=$( array_to_string "${Fedora_additional_packages[@]}" )
    gems=$( array_to_string "${Ruby_gems[@]}")
    sudo dnf -y update && sudo dnf -y upgrade
    sudo dnf -y install $pkgs $add_pkgs
    sudo /usr/bin/gem install $gems
  # In case CentOS or RHEL
  else
    sudo dnf -y install dnf-plugins-core
    if [[ "$DISTRO" == *"CentOS Linux"* ]]; then
      echo "Installing CentOS repos."
      sudo dnf -y install epel-release
      sudo dnf config-manager --set-enabled powertools
    elif [[ "$DISTRO" == *"Red Hat Enterprise Linux"* ]]; then
      echo "Working with RHEL8 repos.."
      sudo subscription-manager repos --enable "codeready-builder-for-rhel-8-$(arch)-rpms"
      sudo dnf -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
    fi
    sudo dnf -y groupinstall "Development Tools" "Additional Development"
    add_pkgs=$( array_to_string "${RHEL_additional_packages[@]}" )
    gems=$( array_to_string "${Ruby_gems_RHEL[@]}")
    sudo dnf -y update && sudo dnf -y upgrade
    sudo dnf -y install $pkgs $add_pkgs
    sudo /usr/bin/gem install $gems
    sudo /usr/bin/gem install open3 -v 0.1.0
  fi 
  
}

install_prereq_Arch ()
{
  pkgs=$( array_to_string "${Arch_packages[@]}" )
  gems=$( array_to_string "${Ruby_gems[@]}" )
  sudo pacman -Syyu --noconfirm $pkgs
  sudo /usr/bin/gem install $gems
}

install_prereq_openSUSE ()
{
  pkgs=$( array_to_string "${openSUSE_packages[@]}" )
  gems=$( array_to_string "${Ruby_gems_RHEL[@]}" )
  sudo zypper refresh && sudo zypper update
  sudo zypper install --type pattern devel_basis
  sudo zypper install --type pattern devel_C_C++
  sudo zypper in $pkgs
  sudo /usr/bin/gem install $gems
  sudo /usr/bin/gem install open3 -v 0.1.0
}

case ${MODE} in
  
  "Ubuntu")
    install_prereq_Ubuntu
    ;;
  "Debian")
    install_prereq_Debian
    ;;
  "Fedora")
    install_prereq_Fedora
    ;;
  "Arch")
    install_prereq_Arch
    ;;
  "openSUSE")
    install_prereq_openSUSE
    ;;
  *)
    echo "${MODE} is not supported now."
    exit 1
    ;;
esac

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

