#!/bin/sh

##############################################################
#                                                            #
# Neofetch install for some un-supported systems.            #
#                                                            #
##############################################################

NEOFETCH_SRC='https://github.com/dylanaraps/neofetch/archive/refs/tags/7.1.0.tar.gz'

TMP_DIR=$(mktemp -d -t neofetch-install-XXXXXXXX)

ORIG_PATH=`pwd`

# Entering temp directory
cd "${TMP_DIR}"

FILE_NAME="${NEOFETCH_SRC##*tags/}"
VER_NUM="${FILE_NAME%%.tar.gz}"
DOWNLOAD_AS="neofetch-${VER_NUM}.tar.gz"

wget "${NEOFETCH_SRC}" -O "${DOWNLOAD_AS}"
tar xvf "./${DOWNLOAD_AS}"
cd ./neofetch-${VER_NUM}

sudo make install

# Returning to current directory...
cd "${ORIG_PATH}"
