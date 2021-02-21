#!/bin/bash

git clone https://gitlab.com/dwt1/shell-color-scripts.git ./shell-color-scripts
cd ./shell-color-scripts && \
  sudo rm -rf /opt/shell-color-scripts && \
  sudo mkdir -pv /opt/shell-color-scripts/colorscripts && \
  sudo cp -rvf colorscripts/* /opt/shell-color-scripts/colorscripts && \
  sudo cp -rvf colorscript.sh /usr/bin/colorscript

# optional for zsh completion
if [ -x "$(command -v zsh)" ]; then
  sudo cp -rfv zsh_completion/_colorscript /usr/share/zsh/site-functions
fi

cd ../ && rm -rfv ./shell-color-scripts
