#!/bin/sh -e

# Setting up some variables to make the job easier...
WORK_DIR="$HOME/.bd3_patch"
ORIG_DIR="$CWD"

GAME_REF_NUM='397540' # Reference number for Borderlands 3 as suggested by Hovercraft

# Mine is Pop! OS, which bases on Ubuntu. It can be different on other machines. Adjust this directory for your own situation
PROTON_DIR="/home/$USER/.steam/debian-installation/steamapps/common/Proton - Experimental/" 

# As I said, I usually gather all the Steam games together in ~/GameAndMedia directory. It will be different in your case. Change it!
WINE_PREFIX_FOR_INSTALL="/home/$USER/GameAndMedia/SteamLibrary/steamapps/compatdata/$GAME_REF_NUM/pfx"

# Game Data directory!
GAME_BIN_DIR="/home/$USER/GameAndMedia/SteamLibrary/steamapps/common/Borderlands 3/OakGame/Binaries/Win64/"

# Good old die function
die () {
  printf 'ERROR! %s\n' "$1"
  exit 1
}

# Installing mf-install
# You need some external tools. 
# if you are on Ubuntu based distro., type
# apt install -y git cabextract 
# if the script complains missing tools.
[ ! -x "$(command -v git)" ] && die "We need git! ponk!"
[ ! -x "$(command -v cabextract)" ] && "We need cabextract!"

# Dealing with work directory
[ -d "$WORK_DIR" ] && rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

# Doing actual job. Installing MF stuffs with z0z0z's repository.
if [ -d "$WORK_DIR" ]; then
  cd "$WORK_DIR" && \
  git clone https://github.com/z0z0z/mf-install && \
  cd "$WORK_DIR/mf-install" && \
  PROTON="$PROTON_DIR" WINEPREFIX="$WINE_PREFIX_FOR_INSTALL" ./mf-install.sh -proton

  cd "$WORK_DIR" && \
  git clone https://github.com/z0z0z/mf-installcab && \
  cd "$WORK_DIR/mf-installcab" && \
  PROTON="$PROTON_DIR" WINEPREFIX="$WINE_PREFIX_FOR_INSTALL" ./install-mf-64.sh

  printf '\nmfplat.dll? will do!'

  cp -rfv "$WORK_DIR/mf-installcab/mfplat.dll" "$GAME_BIN_DIR/"

fi

# Cleaning up!
printf 'Cleaning up everything!\n'
cd "$ORIG_DIR" && rm -rf "$WORK_DIR"

printf 'Jobs finished! Have fun with LOOTING!\n\n\n'
