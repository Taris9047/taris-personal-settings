#!/usr/bin/env bash

if [ $# -eq 0 ]; then
  SYNOPSYS_PATH="/opt/Synopsys"
else
  SYNOPSYS_PATH="$1"
fi

# Synopsys directory
SYNOPSYS_DIR="$SYNOPSYS_PATH"
SYNOPSYS_LICENSE='27000@license.soe.ucsc.edu'
SYNOPSYS_VER='current'

STDB_PE="$HOME/Data/DB"

if [ -d "$SYNOPSYS_DIR" ]; then
  export STDB="$STDB_PE"
  export LM_LICENSE_FILE="$SYNOPSYS_LICENSE"
  export PATH="$PATH:$SYNOPSYS_DIR/$SYNOPSYS_VER/bin"
  export STROOT="$SYNOPSYS_DIR/$SYNOPSYS_VER/"
  export STRELEASE=current
fi
