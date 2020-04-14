#!/bin/sh

#
# Install brew package script for mac environment for me!!
#
# As you know, you need to install Xcode and its command line tools before even
# attempting to install the homebrew. 
#

if [ ! -f "/usr/local/bin/brew" ]; then
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
	echo "Homebrew seems to be already installed on this machine!!"
fi

# The packages to install
PKGS="go bison p7zip lua pypy pypy3 ffmpeg python3 ngspice youtube-dl zeromq emacs flex gnuplot subversion mercurial nano autoconf automake cmake doxygen wget ruby ntfs-3g"

# Now, let's install all those craps!!
brew install $PKGS
brew upgrade

