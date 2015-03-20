#!/bin/bash
USR_DIR=$HOME
CURRENT_DIR=`pwd`
echo "Target directory: ${USR_DIR}"
echo "Source directory: ${CURRENT_DIR}"

# Config Files
CONF_LIST=("vimrc" "gvimrc" "emacs")
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
echo "Manual installation is recommended for .bashrc or .bash_profile depending on your OS."
echo ""
echo "Have a nice day!"
