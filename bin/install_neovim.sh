#!/bin/sh

#
# Let's install neovim to the system.
#
# --> NvChad is the best!!
#

CWD=dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

NVCHAD_GOOD_VER='v0.11.5'
printf 'Current NvChad Recommendation: %s\n' "${NVCHAD_GOOD_VER}"


#
# Checking if nvim is already installed!!
#
if [ -x "$(command -v nvim)" ]; then
  NVIM_VERSION=`nvim --version | awk -F' ' 'NR==1{print $2}'`
  printf 'Neovim %s found in the path!!\n' "${NVIM_VERSION}"
  if [ "${NVIM_VERSION}" = "${NVCHAD_GOOD_VER}" ]; then
    printf 'Guess what? we have a good Neovim for NVCHAD!!\n'
    if [ -x "$(command -v lua)" ]; then
      printf 'We also have lua!!\nYou can install Neovim now!!\n'
    else
      printf 'All that we are missing is lua!!\n'
    fi
  else
    printf 'Not a good Neovim Version... Please re-install correct one.\n'
  fi
  printf 'Exiting without doing anyting... \n'
  exit 0
fi

#
# Check up if we have all the programs...
#
[ ! -x "$(command -v git)" ] && printf 'Error: we need git!!\n' && exit 1
[ ! -x "$(command -v cmake)" ] && printf 'Error: we need cmake!!\n' && exit 1
[ ! -x "$(command -v make)" ] && printf 'Error: we need make!!\n' && exit 1
[ ! -x "$(command -v gcc)" ] && printf 'Error: we need a toolchain: i.e. build-essential\n' && exit 1

# Making work directory...
#
WORK_DIR="$(mktemp -d -t neovim-XXXXXXXXXXXXXXXX)"
if [ ! -d "${WORK_DIR}" ]; then
  printf 'Making work directory: %s\n' "${WORK_DIR}" 
  mkdir -p "${WORK_DIR}"
fi

cd "${WORK_DIR}"

# Cloning git
printf 'Actually doing the compilation stuff\n'
rm -rf "${WORK_DIR}/neovim"
git clone git@github.com:neovim/neovim.git "${WORK_DIR}/neovim" && cd "${WORK_DIR}/neovim" 
git ckeckout "${NVCHAD_GOOD_VER}"
make CMAKE_BUILD_TYPE=RelWithDebInfo
# installs everything to /usr/local/
sudo make install

cd "${CWD}" # Returning to the current directory...
printf 'Deleting the work directory..\n'
rm -rf

printf '\n\n'
printf 'Neovim Installed!!\n'
printf '\n\n'

