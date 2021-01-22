#!/bin/sh

if [ -x "$(command -v cargo)" ]; then
    rustup update
else
	echo "Looks like we don't have Rust! Installing from main repo!"
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
fi

inst_cargo_pkgs ()
{
    pkgs="exa bat rm-improved diskonaut lsd cargo-update starship tokei fd-find procs du-dust ripgrep hyperfine ytop grex zoxide nu"
    cargo install $pkgs
}

inst_cargo_pkgs

