#!/bin/sh

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
  pkgs="exa bat rm-improved diskonaut lsd cargo-update starship tokei fd-find procs du-dust ripgrep hyperfine eureka ddh gitui ytop grex zoxide nu"
  cargo install $pkgs
  cargo install-update -a
}

if [ -d $HOME/.cargo ]; then
  inst_cargo_pkgs
fi

