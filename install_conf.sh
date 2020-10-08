#!/bin/bash
USR_DIR=$HOME
CURRENT_DIR=`pwd`
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
DOTFILESDIR="./dotfiles"
LINUXBASHFILE="$DOTFILESDIR"/bashrc_linux
DARWINBASHFILE="$DOTFILESDIR"/zshrc_osx
if [[ "$PLATFORM" == "linux" || "$PLATFORM" == "cygwin" ]]; then
    cat "$LINUXBASHFILE" >> "$HOME/.bashrc"
    echo "Appending $HOME/.bashrc with $LINUXBASHFILE"
elif [[ "$PLATFORM" == "darwin" ]]; then
    cat "$DARWINBASHFILE" >> "$HOME/.zshrc"
    echo "Appending $HOME/.zshrc with $DARWINBASHFILE"
else
    echo "Manual installation is recommended for .bashrc or .bash_profile depending on your OS."
fi

echo ""
echo "Have a nice day!"
