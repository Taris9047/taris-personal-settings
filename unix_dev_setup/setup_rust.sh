#!/bin/sh

rust_pkgs = ( \
  exa \
  bat \
  rm-improved \
  diskonaut \
  lsd \
  cargo-update \
  starship \
  tokei \
  fd-find \
  procs \
  du-dust \
  ripgrep \
  hyperfine \
  eureka \
  ddh \
  gitui \
  ytop \
  grex \
  zoxide \
  nu \
  broot )

array_to_string ()
{
  arr=("$@")
  echo ${arr[*]}
}

if [ -x "$(command -v cargo)" ]; then
  rustup update
  cargo install-update -a
else
  echo "Looks like we don't have Rust! Installing from main repo!"
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  source $HOME/.bashrc
fi

inst_cargo_pkgs ()
{
  $HOME/.cargo/bin/cargo install $( array_to_string "${rust_pkgs[@]}" )
  # $HOME/.cargo/bin/cargo install-update -a
}

if [ -d $HOME/.cargo ]; then
  inst_cargo_pkgs
fi

