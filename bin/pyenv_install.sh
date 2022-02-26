#!/bin/bash

if [ ! -x "$(command -v pyenv)" ]; then
    printf 'pyenv not found in the path!!\n'
    exit 1
fi

PYENV_CMD="$(command -v pyenv)"

PYCONF_OPTS='--enable-shared --enable-ipv6 --enable-unicode=ucs4 --with-threads --with-ensurepip=yes --enable-optimizations'

PY_CFLAGS='-O2 -march=native -fomit-frame-pointer'

usage () {
    printf 'Python bath installation script using pyenv\n'
    printf '\n'
    printf 'Usage: pyenv_install.sh <python_version1> <python_version2> ...\n'
}

if [ $# -eq 0 ]; then
    usage
    exit 0
fi


# run install

for arg in "$@"
do
    case "$arg" in
        -h | --help)
            usage
            exit 0
            ;;
        *)
            PYTHON_CONFIGURE_OPTS="$PYCONF_OPTS" CFLAGS="$PY_CFLAGS" \
              $PYENV_CMD install -f "$arg"
            ;;
    esac
done
