#!/bin/sh

USR_DIR="/home/$(whoami)"
RBENV_DIR="${USR_DIR}/.rbenv"
RBENV_EXE="${RBENV_DIR}/bin/rbenv"

if [ -x "${RBENV_EXE}" ]; then
  RBENV_ROOT="${RBENV_DIR}"
  PATH="${RBENV_DIR}/bin:${RBENV_DIR}/shims/:${PATH}" 
  eval $(rbenv init - bash)
fi
