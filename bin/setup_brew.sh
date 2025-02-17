#!/bin/bash
#
# Yet another deprecated homebrew script....
#
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

brew install -v python --with-framework
brew install -v wget 
brew install -v gnuplot
brew install -v gforgran

brew install -v gd
brew install -v freetype
brew install -v mcrypt

cd ;
easy_install pip
pip install numpy
pip install scipy
pip install matplotlib

brew install -v --python wxmac 
