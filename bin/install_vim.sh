#!/bin/sh

# Install vim...
#
GITREPO=/tmp/vim

git clone https://github.com/vim/vim.git "${GITREPO}" || exit -1

cd "${GITREPO}" && git pull && ./configure --prefix=/usr/local && make && sudo make install
