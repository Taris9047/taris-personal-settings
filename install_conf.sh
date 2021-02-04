#!/bin/bash
USR_DIR=$HOME
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd -P )"
echo "Target directory: ${USR_DIR}"
echo "Source directory: ${CURRENT_DIR}"

# OS Detection
function lowercase()
{
  echo "$1" | sed "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/"
}

PLATFORM='none'
function set_os_type ()
{
  UNAMESTR=`lowercase \`uname\``
  if [[ "$UNAMESTR" == *"cygwin"* ]]; then
    PLATFORM="cygwin"
  elif [[ "$UNAMESTR" == *"linux"* ]]; then
    PLATFORM="linux"
  elif [[ "$UNAMESTR" == *"darwin"* ]]; then
    PLATFORM="darwin"
  fi
  echo $PLATFORM
}

# Set OS Type
set_os_type
echo "Setting platform as: $PLATFORM"

# Config Files
CONF_LIST=("vim" "vimrc" "emacs" "gitignore" "gitconfig" "gdbinit")
DOT="."
echo ""
for conf_file in ${CONF_LIST[*]}
do
  echo "Installing: ${DOT}${conf_file}"
  rm -rvf $USR_DIR/$DOT$conf_file
  ln -sfv $CURRENT_DIR/dotfiles/$conf_file $USR_DIR/$DOT$conf_file
done

# NVIM
echo "Installing NVIM config file"
NVIM_CONF_HOME=$USR_DIR/.config/nvim
if [ ! -d $NVIM_CONF_HOME ]; then
  echo "NVIM config dir not found, making one."
  mkdir -p $NVIM_CONF_HOME
fi
ln -sfv $CURRENT_DIR/dotfiles/init.vim.nvim $NVIM_CONF_HOME/init.vim

# Starship
echo "Installing starship config file"
STARSHIP_CONF_FILE=$USR_DIR/.config/starship.toml
if [ ! -f $STARSHIP_CONF_FILE ]; then
  echo "starship config file not found, linking one!"
  ln -sfv $CURRENT_DIR/dotfiles/starship.toml $STARSHIP_CONF_FILE
fi

# Some Handy dirs and Symbolic links
GOOGLE_DRIVE=$HOME/".google-drive"
GOOGLE_DRIVE_SYM=$HOME/"GoogleDrive"
ONE_DRIVE=$HOME/".onedrive"
ONE_DRIVE_SYM=$HOME/"OneDrive"
if [ ! -d $GOOGLE_DRIVE ]; then
  mkdir -pv $GOOGLE_DRIVE
  ln -sfv $GOOGLE_DRIVE $GOOGLE_DRIVE_SYM
fi
if [ ! -d $ONE_DRIVE ]; then
  mkdir -pv $ONE_DRIVE
  ln -sfv $ONE_DRIVE $ONE_DRIVE_SYM
fi
HOMEBREW_OPT=$HOMEBREW/.opt
HOMEBREW_OPT_SYM=$HOMEBREW/opt
if [ ! -d $HOMEBREW_OPT ]; then
  mkdir -pv $HOMEBREW_OPT
  ln -sfv $HOMEBREW_OPT $HOMEBREW_OPT_SYM
fi

# Config Directories
# On Linux, this part is unnecessary. However, on OS X or Freebsd..
# ln works differently for directories.
CONF_LIST_D=("emacs.d")
for conf_dir in ${CONF_LIST_D[*]}
do
  echo "Installing: ${conf_dir}"
  rm -rfv $USR_DIR/$DOT$conf_dir
  ln -sfv $CURRENT_DIR/dotfiles/$conf_dir $USR_DIR/$DOT$conf_dir
done


echo ""
echo "**** Note ****"
# Importing bash settings
DOTFILESDIR=$CURRENT_DIR"/dotfiles"
LINUXBASHFILE="$DOTFILESDIR"/bashrc_linux
LINUXZSHFILE="$DOTFILESDIR"/zshrc_linux
DARWINBASHFILE="$DOTFILESDIR"/bash_profile_osx
DARWINZSHFILE="$DOTFILESDIR"/zshrc_osx
SHELL_TYPE="$(echo $0)"

# Now install the shell environments!!
if [[ "$PLATFORM" == "linux" ]]; then
  if [[ "$SHELL_TYPE" == "bash" ]]; then
    echo "Appending $HOME/.bashrc with $LINUXBASHFILE"
    echo "source $LINUXBASHFILE" >> "$HOME/.bashrc"
  elif [[ "$SHELL_TYPE" == "zsh" ]]; then
    echo "Appending $HOME/.zshrc with $LINUXZSHFILE"
    echo "source $LINUXZSHFILE" >> "$HOME/.zshrc"
  elif [[ "$PLATFORM" == "cygwin" ]]; then
    echo "Appending $HOME/.bashrc with $LINUXBASHFILE"
    echo "source $LINUXBASHFILE" >> "$HOME/.bashrc"
  elif [[ "$PLATFORM" == "darwin" ]]; then
    if [[ "$SHELL_TYPE" == "bash" ]]; then
      echo "Appending $HOME/.bash_profile with $DARWINBASHFILE"
      echo "source $DARWINBASHFILE" >> "$HOME/.bash_profile"
    elif [[ "$SHELL_TYPE" == "zsh" ]]; then
      touch "$HOME/.zshrc"
      echo "source $DARWINBASHFILE" >> "$HOME/.zshrc"
      echo "Appending $HOME/.zshrc with $DARWINZSHFILE"
    fi
  fi
fi

echo ""
echo "Have a nice day!"
