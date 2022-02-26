#!/bin/sh

if [ ! -x "$(command -v pyenv)" ]; then
    printf 'pyenv not found in the path!!\n'
    exit 1
fi

PYENV_CMD="$(command -v pyenv)"

PYCONF_OPTS='--enable-shared --enable-ipv6 --enable-unicode=ucs4 --with-threads --with-ensurepip=yes --enable-optimizations'

PY_CFLAGS='-O2 -march=native -fomit-frame-pointer'

if [ -z "$1" ]; then
    printf 'Usage: pyenv_install.sh <python_version>\n'
    exit 0
fi


# run install
PYTHON_CONFIGURE_OPTS="\"$PYCONF_OPTS\"" CFLAGS="\"$PY_CFLAGS\"" \
  $PYENV_CMD install -f "$1"

