#!/bin/sh

# Install vim...
#
TMP_DIR="$(mktemp -d -t vim-XXXXXXXXXXXX)"
WORK_DIR=${TMP_DIR}

MY_DIR=`pwd`

# Checking out vim o tmp directory
git clone https://github.com/vim/vim.git "${GITREPO}" || exit -1

# Compiling vim
cd "${WORK_DIR}" && git pull && ./configure --prefix=/usr/local && make && sudo make install

cd "${MY_DIR}"
