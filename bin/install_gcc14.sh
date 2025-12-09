#!/bin/sh
#
# This is a preliminary gcc install script for Old Ubuntu LTS distributions..
#

URL="http://ftp.gnu.org/gnu/gcc/gcc-14.2.0/gcc-14.2.0.tar.gz"

OLD_DIR="$(pwd -P)"
TMP_DIR="$(mktemp -d -t gcc-14-XXXXXXXXXXX)"

sudo apt update && sudo apt upgrade -y && sudo apt install build-essential libmpfr-dev libgmp3-dev libmpc-dev -y

# Download and extract
#
cd "${TMP_DIR}" && wget "${URL}" && \
    tar -xvf gcc-14.2.0.tar.gz && cd gcc-14.2.0 && \
    ./configure -v --build="$(uname -m)-linux-gnu" -host="$(uname -m)-linux-gnu" --target="$(uname -m)-linux-gnu" --prefix="/usr/local/gcc-14.2.0" --enable-checking=release --enable-languages=c,c++,fortran --disable-multilib --program-suffix=-14 && \
    make -j 8 && \
    sudo make install
