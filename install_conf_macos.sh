#!/bin/sh
set -e

# My settings Initial installation script
printf '**** Setting up Taris'' Environments (MacOS version) ****'

USR_DIR="$HOME"
SETTINGS_DIR="$HOME/.settings"
HOMEBREW="$HOME/.local"
MY_GIT_REPO='git@github.com:Taris9047/taris-personal-settings.git'
MY_UDS_GIT_REPO='git@github.com:Taris9047/uds.git'

printf 'Target directory: %s\n' "${USR_DIR}"
printf 'Source directory: %s\n' "${SETTINGS_DIR}"

# Check git.
[ ! -x "$(command -v git)" ] && printf 'ERROR! No git found!!\n' && exit 1

# Clone settings dir to settings!
[ ! -d "$SETTINGS_DIR" ] && git clone "$MY_GIT_REPO" "$SETTINGS_DIR"
{ [ -d "$SETTINGS_DIR" ] && [ ! -L "$HOME/Settings" ]; } && { ln -sfv "$SETTINGS_DIR" "$HOME/Settings" || true; }

# Make homebrew dir
[ ! -d "$HOMEBREW" ] && mkdir -pv "${HOMEBREW}"
{ [ -d "$HOMEBREW" ] && [ ! -L "$HOME/Homebrew" ]; } && { ln -sfv "$HOMEBREW" "$HOME/Homebrew" || true; }

# OS Detection
lowercase() {
  printf '%s' "$1" | tr '[:upper:]' '[:lower:]'
}

PLATFORM='none'
set_os_type() {
  UNAMESTR=$(lowercase "$(uname)")

  case "$UNAMESTR" in
  *"cygwin"*) PLATFORM="cygwin" ;;
  *"linux"*) PLATFORM="linux" ;;
  *"darwin"*) PLATFORM="darwin" ;;
  *) PLATFORM="general_unix" ;;
  esac

  # printf 'Setting platform as: %s\n' "$PLATFORM"
}

# Set OS Type
set_os_type

if [ "$PLATFORM" != "darwin" ]; then
  printf 'Looks like the system is not OS X. Exiting...\n'
  exit 1
fi

# Implementing a poor man's array in POSIX way.
# referenced: https://github.com/krebs/array/blob/master/array
#
# Array constructor
array() {
  for i in "$@"; do
    printf '%s\n' "$i" | array_element_encode
  done
}

# Array element eocode/decode
array_element_encode() {
  sed 's/%/%25/g' | sed -e :a -e '$!N; s/\n/%0A/; ta'
}

array_element_decode() {
  sed -e 's/%0[aA]/\
/g' -e 's/%25/%/g'
}

# Config Files - This part needs to be POSIXified!
# CONF_LIST=("vimrc" "gitignore" "gitconfig" "gdbinit" "Xresources" "tmux.conf")
CONF_LIST=$(array 'vimrc' 'gitignore' 'gitconfig' 'gdbinit' 'Xresources' 'tmux.conf' 'profile')

# Linking dotfiles
printf "\nLinking dotfiles from settings dir ...\n"
# Iterate with CONF_LIST
#
# Do this stuff for each element
do_inst() {
  printf 'Installing: %s\n' ".${1}"
  rm -rf "${USR_DIR}/.${1}"
  ln -sf "${SETTINGS_DIR}/dotfiles/${1}" "${USR_DIR}/.${1}" || true
}
# Iterate through the array...
printf '%s\n' "$CONF_LIST" |
  while IFS= read -r element; do
    do_inst "$(printf '%s\n' "$element" | array_element_decode)"
  done
# Lastly, prepare the gitconfig.local
[ -L "$HOME/.gitconfig" ] && [ ! -f "$HOME/.gitconfig.local" ] && touch "$HOME/.gitconfig.local"

# Still linking dotfiles... but to some odd locations.
# i.e. .config or .local/config directories etc.
printf '\nWorking on other config files ...\n'
# VIM -- MacOS comes with VIM. Thus installing it.
printf 'Setting up VIM package directory\n'
VIM_CONF_DIR="${USR_DIR}/.vim"
VIM_SETTINGS_DIR="${SETTINGS_DIR}/dotfiles/vim"
[ ! -d "${VIM_CONF_DIR}" ] && rm -rf "$VIM_CONF_DIR"
mkdir -pv "${VIM_CONF_DIR}"
# ln -sf "$VIM_SETTINGS_DIR/autoload" "$VIM_CONF_DIR/autoload"
ln -sf "${VIM_SETTINGS_DIR}/colors" "${VIM_CONF_DIR}/colors"
mkdir -p "${VIM_CONF_DIR}/pack"
mkdir -p "${VIM_CONF_DIR}/terminal_colors"

# NVIM - or Neovim
printf 'Setting up NVIM config file\n'
NVIM_CONF_HOME="${USR_DIR}/.config/nvim"
NVIM_GTK_CONF_HOME="$USR_DIR/.config/nvim-gtk"
#[ ! -d "$NVIM_CONF_HOME" ] && mkdir -pv "$NVIM_CONF_HOME"
#[ ! -d "$NVIM_GTK_CONF_HOME" ] && mkdir -pv "$NVIM_GTK_CONF_HOME"
#rm -rf "$NVIM_CONF_HOME/*init.vim"
#ln -sf "$SETTINGS_DIR/dotfiles/init.vim.nvim" "$NVIM_CONF_HOME/init.vim" || true
#ln -sf "$SETTINGS_DIR/dotfiles/init.vim.nvim" "$NVIM_CONF_HOME/sysinit.vim" || true
#ln -sf "$SETTINGS_DIR/dotfiles/ginit.vim.nvim" "$NVIM_CONF_HOME/ginit.vim" || true
#
# Now installs NvChad instead of copying VIM's setting directly.
#
rm -rf "${NVIM_CONF_HOME}" "${NVIM_GTK_CONF_HOME}"
if [ -d "${USR_DIR}/.local/share/nvim" ]; then
  find "${USR_DIR}/.local/share/nvim/" -maxdepth 1 ! -name "nvim" -prune -name "runtime" -type d -exec rm -rf {} +
fi
#rm -rf "${USR_DIR}/.local/share/nvim"
git clone "https://github.com/NvChad/NvChad" "${HOME}/.config/nvim" --depth 1 

# Starship
printf 'Setting up starship config file\n'
STARSHIP_CONF_FILE="${USR_DIR}/.config/starship.toml"
[ ! -f "${STARSHIP_CONF_FILE}" ] && (ln -sfv "${SETTINGS_DIR}/dotfiles/starship.toml" "${STARSHIP_CONF_FILE}" || true)

# Emacs - Not Doom nor Space. Just Emacs.
if [ ! -d "${HOME}/.doom.d" ]; then
  printf 'Setting up Default emacsd.d\n'
  EMACSD="${HOME}/.emacs.d"
  [ ! -d "${EMACSD}" ] && mkdir -pv "${EMACSD}"
  rm -rf "${EMACSD:?}/*"
  ln -sf "${SETTINGS_DIR}/dotfiles/emacs.d/init.el" "${EMACSD}/init.el" || true
  ln -sf "${SETTINGS_DIR}/dotfiles/emacs.d/config.org" "${EMACSD}/config.org" || true
fi

# Tmux!
printf 'Setting up Tmux Package Manager (TPM)\n'
[ ! -d "${HOME}/.tmux/plugins/tpm" ] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Setting up Orgmode Agenda directory
printf 'Setting up Orgmode directory\n'
ORGMODE_DIR="${HOME}/.org_agenda"
ORGMODE_DIR_SYM="${HOME}/Org"
[ ! -d "${ORGMODE_DIR}" ] && mkdir -pv "${ORGMODE_DIR}"
rm -rf "${ORGMODE_DIR_SYM}"
ln -sf "${ORGMODE_DIR}" "${ORGMODE_DIR_SYM}"

append_string() {
  if ! grep -Fxq "${2}" "${1}"; then
    # printf 'Appending %s with %s\n' "${2}" "${1}"
    printf "\n\n%s\n" "${2}" >>"${1}"
  fi
}
append_source() {
  append_string "${1}" ". ${2}"
}

printf 'Setting up shell environments\n'
# Importing bash settings
DOTFILESDIR="$SETTINGS_DIR/dotfiles"
LINUXBASHFILE="$DOTFILESDIR/bashrc_linux"
LINUXZSHFILE="$DOTFILESDIR/zshrc_linux"
DARWINBASHFILE="$DOTFILESDIR/bash_profile_osx"
DARWINZSHFILE="$DOTFILESDIR/zshrc_osx"
# SHELL_TYPE="$(echo $SHELL)"

inst_env_linux() {
  [ ! -f "$HOME/.bashrc" ] && touch "$HOME/.bashrc"
  append_source "$HOME/.bashrc" "$LINUXBASHFILE"

  [ ! -f "$HOME/.zshrc" ] && touch "$HOME/.zshrc"
  append_source "$HOME/.zshrc" "$LINUXZSHFILE"
}

inst_env_cygwin() {
  append_source "$HOME/.bashrc" "$LINUXBASHFILE"
}

inst_env_darwin() {
# [ ! -f "$HOME/.bash_profile" ] && touch "$HOME/.bash_profile"
# append_source "$HOME/.bash_profile" "$DARWINBASHFILE"

  [ ! -f "$HOME/.zshrc" ] && touch "$HOME/.zshrc"
  append_source "$HOME/.zshrc" "$DARWINZSHFILE"
}

#
# Now install the shell environments!!
#
case "$PLATFORM" in
"linux")
  inst_env_linux
  ;;

"cygwin")
  inst_env_cygwin
  ;;

"darwin")
  inst_env_darwin
  ;;

"*") # Considering general Unix OS as Linux
  inst_env_linux
  ;;
esac

# Downloading unix_dev_setup
# --> Apparently, you have new enough git and other fileutils if you have already downloaded this file!
printf 'Setting up Unix Development Setup to %s\n' "$HOME/.uds"
UDS_DIR="$HOME/.uds"
UDS_LNK="$SETTINGS_DIR/unix_dev_setup"
if [ ! -d "$UDS_DIR" ]; then
  git clone "$MY_UDS_GIT_REPO" "$UDS_DIR"
  rm -rf "$UDS_LNK"
  ln -sf "$UDS_DIR" "$UDS_LNK"
fi

# Installing rust stuffs
#
# Rust tool list
# --> Note that some rust tools cannot be compiled on MacOS due to 
# lack of convenient openssl library link.
# We need to handle it for some way.
#
RUST_LIST=$(array 'exa' 'bat' 'rm-improved' 'diskonaut' 'ripgrep' 'fd-find' 'tokei' 'lsd' 'procs' 'hjson' 'eza')

# Rust tool install
do_rust_inst () {
  CMD_TO_INST="${1}"
  CARGO_BIN="$(command -v cargo)"
  if [ "${1}" = "fd-find" ]; then
    CMD_TO_INST="fd"
  elif [ "${1}" = "ripgrep" ]; then
    CMD_TO_INST="rg"
  elif [ "${1}" = "rm-improved" ]; then
    CMD_TO_INST="rip"
  fi
  
  if [ -z "$(command -v ${CMD_TO_INST})" ]; then
    "${CARGO_BIN}" install "${1}"
  fi
}

# Install Cargo
if [ -z "$(command -v cargo)" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

# Install Rust tools
printf '%s\n' "${RUST_LIST}" |
  while IFS= read -r element; do
    do_rust_inst "$(printf '%s\n' "$element" | array_element_decode)"
  done

# Installing Starship
if [ -z "$(command -v starship)" ]; then
  CARGO_BIN="$(command -v cargo)"
  # Starship cannot be installed with cargo.. Unless openssl is installed with brew.
  # So, just taking the binary way...
  #"${CARGO_BIN}" install starship --locked
  curl -sS https://starship.rs/install.sh | sh
fi

#
# Node.JS
#
if [ -z "$(command -v node)" ]; then
  # Install command from 'nodejs.org/en/download' as of Feb. 2025
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
  nvm install 22
  printf 'node+npm installation finished!\n'
fi

#
# Setting up Homebrew: Yes, you don't need my crappy install scripts for MacOS!!
#
if [ -z "$(command -v brew)" ]; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  #append_source 'eval "$(/opt/homebrew/bin/brew shellenv)"' "${HOME}/.zprofile"
  if [ ! -f "${HOME}/.zprofile" ]; then
    touch "${HOME}/.zprofile"
  fi
  (echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"') >> "${HOME}/.zprofile"
fi
[ -f "/opt/homebrew/bin/brew" ] && eval "$(/opt/homebrew/bin/brew shellenv)"

# List of Brew packages to install
printf 'Installing Brew Packages\n'
BREW_PKGS=$(array 'wget' 'emacs' 'openssh' 'neovim' 'gnuplot' 'cmake' 'flex' 'bison' 'neofetch' 'figlet' 'lolcat' 'duf' 'golang' 'tmux' 'btop' 'htop' 'meld' 'spark' 'fzf')
do_brew_inst() {
  BREW_CMD="$(command -v brew)"
  if [ ! -z "${BREW_CMD}" ]; then 
    if [ "${1}" = "emacs" ]; then
      "${BREW_CMD}" tap d12frosted/emacs-plus
      "${BREW_CMD}" install emacs-plus
    elif [ "${1}" = "meld" ]; then
      "${BREW_CMD}" install --cask "${1}"
    else
      "${BREW_CMD}" install "${1}"
    fi
  fi
}
printf '%s\n' "${BREW_PKGS}" |
  while IFS= read -r element; do
    do_brew_inst "$(printf '%s\n' "$element" | array_element_decode)"
  done

# Setting up NVChad
if [ -x "$(command -v nvim)" ]; then
  printf 'Setting up NVIM config files for NvChad\n'
  NVIM_CONF_HOME="${USR_DIR}/.config/nvim"
  NVIM_GTK_CONF_HOME="$USR_DIR/.config/nvim-gtk"
  rm -rf "${NVIM_CONF_HOME}" "${NVIM_GTK_CONF_HOME}"
  find "${USR_DIR}/.local/share/nvim/" -maxdepth 1 ! -name "${USR_DIR}/.local/share/nvim/" -prune -name "runtime" -type d -exec rm -rf {} +
  #rm -rf "${USR_DIR}/.local/share/nvim"
  git clone "https://github.com/NvChad/starter" "${HOME}/.config/nvim" --depth 1 
fi

# Tmux!
printf 'Setting up Tmux Package Manager (TPM)\n'
[ ! -d "$HOME/.tmux/plugins/tpm" ] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm


printf '\n'
printf '\n**** Closing Comments ****\n'
printf 'Run %s dir to set up Doomemacs stuffs.\n\n' "$SETTINGS_DIR/bin/install_doomemacs.sh"

if [ -z "${HOME}/.gitconfig.local" ]; then
  printf 'TODO: Also, don''t forget to populate %s\n\n' "$HOME/.gitconfig.local"
fi

if [ -x "$(command -v tmux)" ]; then
  printf 'Sometimes tmux does not run install packages automatically: Ctrl+B Shift+I will do it manually.\n'

printf 'Have a nice day!\n\n'
