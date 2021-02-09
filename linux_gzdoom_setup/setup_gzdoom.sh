#!/bin/bash

# Install location
PREFIX="$HOME/.local"

# Work dirs
CWD=`pwd -P`
WORKSPACE="$CWD/workspace"
SOURCE="$CWD/src"

# Default compiler settings
CC=$(command -v gcc)
CXX=$(command -v g++)
LDFLAGS="-Wl,-rpath=$PREFIX/lib"
CFLAGS="-I$PREFIX/include -O3 -march=native -fomit-frame-pointer -pipe"
CXXFLAGS="$CFLAGS"

# Some functions
make_dir () {
  if [ ! -d "$1" ]; then
    if ! mkdir -pv "$1"; then
      printf "\n Failed to create dir %s" "$1";
      exit 1
    fi
  fi
}

remove_dir () {
  if [ -d "$1" ]; then
    rm -rvf "$1"
  fi
}

cleanup () {
  remove_dir "$WORKSPACE"
  remove_dir "$SOURCE"
}

execute () {
  echo "$ $*"

  OUTPUT="$(eval "$@" 2>&1)"

  # shellcheck disable=SC2181
  if [ $? -ne 0 ]; then
    echo "$OUTPUT"
    echo ""
    echo " Failed to Execute $*" >&2
    exit 1
  fi
}

# Menu stuff
case "$1" in
  "--cleanup")
    cleanup
    echo "Cleanup done."
    echo ""
    exit 0
    ;;
  "--build")
    echo "Using GCC Toolset..."
    ;;
  "--with-clang")
    CC=$(command -v clang)
    CXX=$(command -v clang++)
    if [ -x $CC ] && [ -x $CXX ]; then
      echo "Using Clang Toolset"
    else
      CC=$(command -v gcc)
      CXX=$(command -v g++)
      echo "Clang toolset was not found! Reverting back to GCC Toolset."
    fi
    ;;
  *)
    echo "Usage: $0"
    echo "  --build: start building stuff."
    echo "  --with-clang: build with clang. Except ZDL."
    echo "  --cleanup: remove all source files and build artifacts."
    echo "  --help: show this message."
    echo ""
    exit 0
    ;;
esac

# LOGO
echo "***************************************************"
echo "*                                                 *"
echo "* GZDoom hombrew script for Linux.                *"
echo "* Only tested on Ubuntu 20.04 based distro.       *"
echo "*                                                 *"
echo "***************************************************"
echo ""

# Installing prereqs.
echo "***************************************************"
echo "*                                                 *"
echo "* Installing prereq packages...                   *"
echo "*                                                 *"
echo "***************************************************"
echo ""
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

array_to_string ()
{
  arr=("$@")
  echo ${arr[*]}
}

Ubuntu_packages=(g++ make cmake libsdl2-dev git zlib1g-dev \
  libbz2-dev libjpeg-dev libfluidsynth-dev libgme-dev libopenal-dev \
  libmpg123-dev libsndfile1-dev libgtk-3-dev timidity nasm \
  libgl1-mesa-dev tar libsdl1.2-dev libglew-dev qtbase5-dev)

Fedora_packages=(gcc-c++ make cmake SDL2-devel git zlib-devel bzip2-devel \
  libjpeg-turbo-devel fluidsynth-devel game-music-emu-devel openal-soft-devel \
  libmpg123-devel libsndfile-devel gtk3-devel timidity++ nasm \
  mesa-libGL-devel tar SDL-devel glew-devel qt5-qtbase qt5-qtbase-devel)

Arch_packages=(gcc make cmake sdl2 git zlib bzip2 libjpeg-turbo \
  fluidsynth libgme openal mpg123 libsndfile gtk3 timidity++ nasm \
  mesa glu tar sdl glew qt5-base qt5-tools)

if [ "$MODE" == "Ubuntu" ]; then
  sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y install $( array_to_string "${Ubuntu_packages[@]}" )
elif [ "$MODE" == "Fedora" ]; then
  sudo dnf -y update && sudo dnf -y upgrade && sudo dnf -y install $( array_to_string ${Fedora_packages[@]} )
elif [ "$MODE" == "Arch" ]; then
  sudo pacman -Syuu $( array_to_string ${Arch_packages[@]} )
fi

# Now, let's really install stuffs
echo "***************************************************"
echo "*                                                 *"
echo "* Building GZDoom from Git                        *"
echo "*                                                 *"
echo "***************************************************"
echo ""

# Env Var NUMJOBS overrides automatic detection
if [[ -n $NUMJOBS ]]; then
  MJOBS=$NUMJOBS
elif [[ -f /proc/cpuinfo ]]; then
  MJOBS=$(grep -c processor /proc/cpuinfo)
elif [[ "$OSTYPE" == "darwin"* ]]; then
  MJOBS=$(sysctl -n machdep.cpu.thread_count)
  CONFIGURE_OPTIONS+=("--enable-videotoolbox")
else
  MJOBS=4
fi
echo ">> Number of jobs set for $MJOBS"

make_dir $SOURCE
make_dir $WORKSPACE

# First, we need to setup ZMusic
echo ""
echo ">>>>> Working on ZMusic"
echo ""

ZMUSIC_SRC="$SOURCE/ZMusic-src"
ZMUSIC_BUILD="$WORKSPACE/ZMusic-build"
if [ ! -f $WORKSPACE/ZMusic.lck ]; then
  execute cd $SOURCE && git clone https://github.com/coelckers/ZMusic.git $ZMUSIC_SRC
  remove_dir $ZMUSIC_BUILD
  make_dir $ZMUSIC_BUILD
  cd $ZMUSIC_BUILD && \
    cmake $ZMUSIC_SRC \
    -DCMAKE_INSTALL_PREFIX="$PREFIX" \
    -DCMAKE_C_COMPILER="$CC" \
    -DCMAKE_CXX_COMPILER="$CXX" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS="$CFLAGS" \
    -DCMAKE_CXX_FLAGS="$CXXFLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LDFLAGS" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LDFLAGS" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS" \
    && make -j $MJOBS \
    && make install
  touch $WORKSPACE/ZMusic.lck
else
  echo "ZMusic.lck found! Skipping."
fi

# Then the GZDoom itself!
echo ""
echo ">>>>> Working on GZDoom!!"
echo ""

GZDOOM_SRC="$SOURCE/gzdoom-src"
GZDOOM_BUILD="$WORKSPACE/gzdoom-build"
if [ ! -f $WORKSPACE/gzdoom.lck ]; then
  cd $SOURCE && git clone git://github.com/coelckers/gzdoom.git $GZDOOM_SRC
  cd $GZDOOM_SRC && git config --local --add remote.origin.fetch +refs/tags/*:refs/tags/* && git pull
  remove_dir $GZDOOM_BUILD
  make_dir $GZDOOM_BUILD
  cd $GZDOOM_BUILD && \
    cmake $GZDOOM_SRC \
    -DCMAKE_INSTALL_PREFIX="$PREFIX" \
    -DCMAKE_C_COMPILER="$CC" \
    -DCMAKE_CXX_COMPILER="$CXX" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS="$CFLAGS" \
    -DCMAKE_CXX_FLAGS="$CXXFLAGS" \
    -DCMAKE_EXE_LINKER_FLAGS="$LDFLAGS" \
    -DCMAKE_MODULE_LINKER_FLAGS="$LDFLAGS" \
    -DCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS" \
    -DZMUSIC_INCLULDE_DIR="$PREFIX/include" \
    -DZMUSIC_LIBRARIES="$PREFIX/lib/libzmusic.so" \
    && make -j $MJOBS
  if [ ! -w $PREFIX ]; then
    cd $GZDOOM_BUILD && sudo make install
  else
    cd $GZDOOM_BUILD && make install
  fi
  touch $WORKSPACE/gzdoom.lck
else
  echo "gzdoom.lck found! skipping..."
fi

# Working on ZDL
ZDL_SRC="$SOURCE/zdl-src"
ZDL_BUILD="$WORKSPACE/zdl-build"
if [ ! -f $WORKSPACE/zdl.lck ]; then
  cd $SOURCE && git clone https://github.com/qbasicer/qzdl.git $ZDL_SRC
  remove_dir $ZDL_BUILD
  make_dir $ZDL_BUILD
  cd $ZDL_BUILD && \
    cmake $ZDL_SRC \
    -DCMAKE_INSTALL_PREFIX="$PREFIX" \
    -DCMAKE_C_COMPILER="$(command -v gcc)" \
    -DCMAKE_CXX_COMPILER="$(command -v g++)" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_EXE_LINKER_FLAGS="$LDFLAGS" \
    && make -j $MJOBS && \
    cp -vfr ./zdl $PREFIX/bin/
  touch $WORKSPACE/zdl.lck
else
  echo "zdl.lck found! skipping!"
fi

echo "***************************************************"
echo "*                                                 *"
echo "* GZDoom/ZDL(Qt) compilation done!                *"
echo "*                                                 *"
echo "* Have fun setting them up!!!!!                   *"
echo "*                                                 *"
echo "***************************************************"
echo ""
