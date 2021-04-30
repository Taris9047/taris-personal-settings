#!/bin/bash

prefix_dir="$HOME/.local"

if [ -d ./shell-color-scripts ]; then
  rm -rf ./shell-color-scripts
fi

git clone https://gitlab.com/dwt1/shell-color-scripts.git ./shell-color-scripts

cd ./shell-color-scripts && \
  rm -rf $prefix_dir/opt/shell-color-scripts && \
  mkdir -pv $prefix_dir/opt/shell-color-scripts/colorscripts && \
  cp -rvf colorscripts/* $prefix_dir/opt/shell-color-scripts/colorscripts && \
  cp -rvf colorscript.sh $prefix_dir/bin/colorscript && \
  chmod +x $prefix_dir/bin/colorscript

# optional for zsh completion
if [ -x "$(command -v zsh)" ]; then
  cp -rfv zsh_completion/_colorscript $prefix_dir/share/zsh/site-functions
fi

cd ../ && rm -rfv ./shell-color-scripts
