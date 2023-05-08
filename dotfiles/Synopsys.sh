#!/usr/bin/env bash

# Synopsys directory
SYNOPSYS_DIR="/opt/Synopsys"
SYNOPSYS_LICENSE='27000@license.soe.ucsc.edu'
SYNOPSYS_VER='K_2015.06-SP2'

if [ -d "$SYNOPSYS_DIR" ]; then
  export STDB="$SYNOPSYS_DIR/DB"
  export LM_LICENSE_FILE="$SYNOPSYS_LICENSE"
  export PATH="$SYNOPSYS_DIR/$SYNOPSYS_VER/bin:$PATH"
  export STROOT="$SYNOPSYS_DIR/$SYNOPSYS_VER/"
  export STRELEASE=current
fi
