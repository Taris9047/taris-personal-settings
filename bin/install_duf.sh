#!/bin/sh -e

#
# Duf installation script.
#

GIT_REPO='https://github.com/muesli/duf.git'

PREFIX='/usr/local'

if [ ! -x "$(command -v go)" ]; then
    printf 'Golang not found in path!!\n'
    printf 'Please install golang and set up PATH for it\n'
    exit 1
fi

WORK_DIR="$(mktemp -d -t duf-XXXXXXXXXXXXXXx)"
cd "${WORK_DIR}"

git clone "${GIT_REPO}" ./duf && cd ./duf && GOPATH=$HOME/.go go build && sudo cp -rfv ./duf "${PREFIX}/bin/"

printf 'Duf Installed!!\n'
