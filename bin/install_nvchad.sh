#!/bin/sh
#
# NvChad installation script
#

#
# Removing previous NvChad
#
remove_NvChad () {
  rm -rfv ~/.config/nvim
  rm -rfv ~/.local/state/nvim
  rm -rfv ~/.local/share/nvim

  # Removing flatpak version too...
  rm -rfv ~/.var/app/io.neovim.nvim/config/nvim
  rm -rfv ~/.var/app/io.neovim.nvim/data/nvim
  rm -rfv ~/.var/app/io.neovim.nvim/.local/state/nvim
}

remove_NvChad


#
# Checking prereqs..
#
if [ ! -x "$(command -v git)" ]; then
  printf 'git is a must have to install NvChad.\n'
fi

if [ ! -x "$(command -v nvim)" ]; then
  printf 'Obviously, we need neovim on the system..\n'
fi

if [ ! -x "$(command -v lua)" ]; then
  printf 'We need lua on the system as well...\n'
fi

#
# Actually Installing the NvChad
#
git clone https://github.com/NvChad/starter ~/.config/nvim && nvim

