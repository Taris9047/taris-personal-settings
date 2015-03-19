#!/bin/sh
echo "This script might invoke superuser password input. Yes, it uses sudo."
echo "So, if you are not listed in 'sudo' list, this script won't work."
echo ""
echo "Updating homebrew."
brew update && brew outdated
brew upgrade
echo ""
echo "It doesn't matter whether brew is update or not!" 
echo "Updating OS X - requires superuser privilege!"
sudo softwareupdate --install --all
echo "Note that this update only covers security patch from Apple. To update softwares from the AppStore, you need to do it manually."
