#!/bin/sh

#
# A simple scipt to install BTop
#

URL='https://github.com/aristocratos/btop.git'

OLD_DIR=`pwd`
WORK_DIR="$(mktemp -d -t btop-XXXXXXXX)"

cd "${WORK_DIR}"

git clone "${URL}" "${WORK_DIR}/btop"
cd "${WORK_DIR}/btop"

make && sudo make install


cd "${OLD_DIR}"
