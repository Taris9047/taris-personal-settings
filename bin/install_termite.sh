#!/usr/bin/env bash

# A script to install the old termite... which is quite good, still.
#
# The motivation was Alacritty needs an OpenGL enabled GPU... 
# which most virtual machines do not have. 
# Also, sometimes, GPU driver set up can be hectic.
#
#
# Adopted from: https://github.com/Corwind/termite-install
#
# Still, installing termite on other distribution is the only 
# script...
#

die () {
    [ -z "$1" ] && exit 1
    printf 'ERROR: %s\n' "$1"
    exit 1 
}

# Cleaning up previous dirs...
[ -d "./vte-ng" ] && rm -rf ./vte-ng
[ -d "./termite" ] && rm -rf ./termite

sudo apt-get install -y \
	git \
	g++ \
	libgtk-3-dev \
	gtk-doc-tools \
	gnutls-bin \
	valac \
	intltool \
	libpcre2-dev \
	libglib3.0-cil-dev \
	libgnutls28-dev \
	libgirepository1.0-dev \
	libxml2-utils \
	gperf || die "Apt package installation error!"
	
git clone --recursive https://github.com/thestinger/termite.git
git clone https://github.com/Taris9047/vte-ng.git

echo export LIBRARY_PATH="/usr/include/gtk-3.0:$LIBRARY_PATH"
cd vte-ng && ./autogen.sh && make && sudo make install || die "vte-ng compile failed!"
cd ../termite && make && sudo make install || die "termite compile failed!"
sudo ldconfig
sudo mkdir -p /lib/terminfo/x; sudo ln -sf \
/usr/local/share/terminfo/x/xterm-termite \
/lib/terminfo/x/xterm-termite

sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/termite 60

sudo rm -rf ./vte-ng ./termite

printf 'Jobs finished!!\n\n'


