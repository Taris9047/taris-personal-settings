#!/bin/sh

#
# Golang source install script...
#

# Match those versions according to GO installation manual.
#
# https://go.dev/doc/install/source
#
GO_VERSION=1.25.5
GO_BOOTSTRAP_VERSION=1.22.12

URL_GO="https://go.dev/dl/go${GO_VERSION}.src.tar.gz"
GO_PREFIX='/usr/local'
URL_BOOTSTRAP="https://go.dev/dl/go${GO_BOOTSTRAP_VERSION}.linux-amd64.tar.gz"

# at first, deleting previous go
sudo rm -rf "${GO_PREFIX}/go"

WORK_DIR="$(mktemp -d -t golang-XXXXXXXXXX)"
CWD=`pwd`
cd "${WORK_DIR}"

wget "${URL_BOOTSTRAP}" 
tar xf ./go${GO_BOOTSTRAP_VERSION}.linux-amd64.tar.gz && mv ./go ./go-bootstrap

# Now set up BOOTSTRAP Go PATH
GO_BOOTSTRAP_PATH="${WORK_DIR}/go-bootstrap"

# Downloading the actual Go version
wget "${URL_GO}" && tar xf ./go${GO_VERSION}.src.tar.gz && cd ./go/src && GOROOT_BOOTSTRAP="${GO_BOOTSTRAP_PATH}" ./make.bash

cd "${WORK_DIR}" && sudo cp -vfr ${WORK_DIR}/go ${GO_PREFIX}/

printf 'Golang installation finished. Make sure add GOROOT and PATH accordingly\n'
printf 'GOROOT=%s\n' "${GO_PREFIX}/go"
printf 'PATH=%s${PATH}\n' "${GO_PREFIX}/go/bin"

cd ${CWD}
