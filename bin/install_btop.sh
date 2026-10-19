#!/bin/sh

#
# A simple scipt to install BTop
#

URL='https://github.com/aristocratos/btop.git'
BIN_URL='https://github.com/aristocratos/btop/releases/download/v1.4.7/btop-arm-unknown-linux-musleabi.tar.gz'

sudo rm -rf /tmp/btop-*

OLD_DIR=`pwd`
WORK_DIR="$(mktemp -d -t btop-XXXXXXXX)"

# TODO: Streamline the compilation tool detection
S_CC=gcc
S_CXX=g++

# Detect if gcc-14 exists - The minimum requirement for btop at the moment. 
detect_gcc14 () {
  if [ -x "$(command -v gcc-14)" ]; then
    printf 'GCC-14 found in the system. Trying with this compiler!\n'
    printf 'It might crash!!\n'
    S_CC="$(command -v gcc-14)"
    S_CXX="$(command -v g++-14)"
  else
    printf 'G++-14 was not found in the system. But we will try with system g++ anyway.\n'
    printf 'BTOP++ needs GCC/G++-14 at least to compile install.\n'
  fi
}

# Functions to install BTOP depending on situation.
#
# Binary installation for ARM platform, such as Raspberry PI
inst_bin () {
  cd "${WORK_DIR}"

  wget "${BIN_URL}" -O ./btop.tbz
  tar xvf ./btop.tbz
  cd ./btop/

  sudo mkdir -p /usr/local
  sudo ./install.sh

  cd "${OLD_DIR}"
}

# Compilation install for modern Linux
inst_src () {
  detect_gcc14
  cd "${WORK_DIR}"
  
  git clone "${URL}" "${WORK_DIR}/btop"
  cd "${WORK_DIR}/btop"
  CC=${S_CC} CXX=${S_CXX} make ADDFLAGS=-march=native && sudo make install

  cd "${OLD_DIR}"
}

PLATFORM=''
detect_platform () {
  # Fetch the raw machine architecture name
  ARCH=$(uname -m)

  case "$ARCH" in
    # x86 Architectures (64-bit and 32-bit variants)
    x86_64 | amd64)
      PLATFORM="x86 (64-bit)"
      inst_src
      ;;
    i386 | i486 | i586 | i686)
      PLATFORM="x86 (32-bit)"
      inst_src
      ;;
        
    # ARM Architectures (64-bit and 32-bit variants)
    aarch64 | arm64)
      PLATFORM="ARM (64-bit)"
      inst_bin
      ;;
    arm* | aarch32)
      PLATFORM="ARM (32-bit)"
      inst_bin
      ;;
        
    # Fallback for alternative or unlisted architectures
    *)
      PLATFORM="Unknown Architecture ($ARCH)"
      ;;
  esac
}

# Detecting PLATFORM
detect_platform

echo "Platform detected: ${PLATFORM}" 


