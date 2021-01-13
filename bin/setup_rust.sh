#!/bin/sh

if ! [ -x $(command -v cargo) ]; then
    rustup update
else
	echo "Looks like we don't have Rust! Installing from main repo!"
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

inst_cargo_pkgs ()
{
    pkgs="exa lsd bat cargo-update diskonaut rm-improved starship tokei"
    cargo install $pkgs
}

inst_cargo_pkgs

