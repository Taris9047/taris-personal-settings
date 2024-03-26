#!/bin/sh

USR_DIR="/home/$(whoami)"
RBENV_DIR="${USR_DIR}/.rbenv"
RBENV_EXE="${RBENV_DIR}/bin/rbenv"

if [ -x "${RBENV_EXE}" ]; then
  export RBENV_ROOT="${RBENV_DIR}"
  export PATH="${RBENV_DIR}/bin:${RBENV_DIR}/shims/:${PATH}" 
  eval $(rbenv init - bash)
fi
