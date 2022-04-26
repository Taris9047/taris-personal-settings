#!/bin/sh

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
new_git_ver='2.36.0'
GIT_PREFIX="$HOME/.local"

n_proc="$(cat /proc/cpuinfo | grep -ic "Processor")"

install_git() {

	[ ! -x "$(command -v gcc)" ] && die "ouch! no gcc?"

	# echo "Trying to delete old git from package..."
	# sudo -H apt -y remove git && sudo -H apt autoremove || true

	echo "Ok, let's install real git!!"

	CWD=$(pwd -P)

	local bld_dir="${SCRIPTPATH}/.build"

    if [ -n "$1" ]; then
        GIT_PREFIX="$1"
    fi

	mkdir -pv "$bld_dir" &&
		cd "$bld_dir" &&
		wget "https://www.kernel.org/pub/software/scm/git/git-${new_git_ver}.tar.xz" -O "${bld_dir}/git-${new_git_ver}.tar.xz" &&
		tar xpvf "${bld_dir}/git-${new_git_ver}.tar.xz" &&
		cd "${bld_dir}/git-${new_git_ver}/" &&
		./configure --prefix="$GIT_PREFIX" &&
		make -j${n_proc} && make install &&
		cd "$CWD" &&
		rm -rf "$bld_dir"
	echo "Ok, Installed new git on $GIT_PREFIX!!"
} # install_git()

install_git
