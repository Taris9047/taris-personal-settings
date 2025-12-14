#!/bin/sh

#
# A simple scipt to install BTop
#

URL='https://github.com/aristocratos/btop.git'
BIN_URL='https://github.com/aristocratos/btop/releases/download/v1.4.5/btop-x86_64-linux-musl.tbz'

sudo rm -rf /tmp/btop-*

OLD_DIR=`pwd`
WORK_DIR="$(mktemp -d -t btop-XXXXXXXX)"

S_CC=gcc
S_CXX=g++

# Detect if gcc-14 exists 
if [ -x "$(command -v gcc-14)" ]; then
    printf 'GCC-14 found in the system. Trying with this compiler!\n'
    printf 'It might crash!!\n'
    S_CC="$(command -v gcc-14)"
    S_CXX="$(command -v g++-14)"
fi


cd "${WORK_DIR}"

if [ $# -eq 0 ]; then

    #
    # Binary install
    #
    wget "${BIN_URL}" -O ./btop.tbz
    tar xvf ./btop.tbz
    cd ./btop/

    sudo mkdir -p /usr/local
    sudo ./install.sh

else
    
    while [ $# -ne 0 ]
    do
        arg="$1"
        case "$arg" in
            -src)
                #
                # Compile install - Does not work on Ubuntu LTS
                # GCC requirement is 14... Pretty hard on LTS 20.04 or 22.04
                #
                git clone "${URL}" "${WORK_DIR}/btop"
                cd "${WORK_DIR}/btop"

                CC=${S_CC} CXX=${S_CXX} make ADDFLAGS=-march=native && sudo make install
                ;;
            *)
                break
                ;;
        esac
        shift
    done

fi


cd "${OLD_DIR}"
