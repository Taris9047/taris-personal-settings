#!/bin/sh

# Install vim...
#
TMP_DIR="$(mktemp -d -t vim-XXXXXXXXXXXX)"
WORK_DIR=${TMP_DIR}
COMPILE_DIR="${WORK_DIR}/vim"

MY_DIR=`pwd`

# Checking out vim o tmp directory
cd "${WORK_DIR}" && git clone https://github.com/vim/vim.git "${COMPILE_DIR}" || exit -1

# Compiling vim
cd "${COMPILE_DIR}" && git pull && ./configure --prefix=/usr/local && make && sudo make install

cd "${MY_DIR}"
