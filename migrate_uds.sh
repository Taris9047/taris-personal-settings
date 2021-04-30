#!/bin/sh

USR_DIR=$HOME
HOMEBREW=$HOME/.local
UDS_GIT_REPO='git@github.com:Taris9047/uds.git'
#SETTINGS_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SETTINGS_PATH=$(cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P)

# Downloading unix_dev_setup
# --> Apparently, you have new enough git and other fileutils if you have already downloaded this file!
UDS_DIR="$HOME/.uds"
UDS_LNK="$SETTINGS_PATH/unix_dev_setup"
[ ! -d "$UDS_DIR" ] && git clone "$UDS_GIT_REPO" "$UDS_DIR"
ln -sfv "$UDS_DIR" "$UDS_LNK"

echo ""
echo "Unix Dev Setup Installed!!"
echo "Have a nice day!"
echo ""
