#!/bin/sh

XILINX_DIR="/opt/Xilinx/"
XILINX_VERSION="2022.2"

if [ -d "$XILINX_DIR" ]; then
    export XILINX="$XILINX_DIR"
    export PATH=$PATH:$XILINX/Vivado/$XILINX_VERSION/bin
    export PATH=$PATH:$XILINX/Vitis/$XILINX_VERSION/bin
    # export PATH=$PATH:/opt/Xilinx/Vitis_HLS/$XILINX_VERSION/bin
fi

