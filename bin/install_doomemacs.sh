#!/bin/bash -e

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

# Minimum allowed utility versions
git_minimal='2.23.0'
new_git_ver='2.48.1'
ripgrep_min_ver='11.0'
fd_min_ver='7.3.0'

version_greater_equal() {
	printf '%s\n%s\n' "$2" "$1" | sort -V -C
}

die() {
	printf 'ERROR: %s\n' "$1"
	return 1
}

install_git_ubuntu() {
	printf 'Installing newest git from PPA!!\n'
	sudo add-apt-repository ppa:git-core/ppa
	sudo apt-get update && sudo apt-get -y upgrade
}

install_git() {

	[ ! -x "$(command -v gcc)" ] && die "ouch! no gcc?"

	# echo "Trying to delete old git from package..."
	# sudo -H apt -y remove git && sudo -H apt autoremove || true

	echo "Ok, let's install real git!!"

	CWD=$(pwd -P)

	local bld_dir="${SCRIPTPATH}/.build"

	mkdir -pv "$bld_dir" &&
		cd "$bld_dir" &&
		wget "https://www.kernel.org/pub/software/scm/git/git-${new_git_ver}.tar.xz" -O "${bld_dir}/git-${new_git_ver}.tar.xz" &&
		tar xpvf "${bld_dir}/git-${new_git_ver}.tar.xz" &&
		cd "${bld_dir}/git-${new_git_ver}/" &&
		./configure --prefix="$HOME"/.local &&
		make -j2 && make install &&
		cd "$CWD" &&
		rm -rf "$bld_dir"
	echo "Ok, Installed new git on /usr/local !!"
} # install_git()

# Doing some prereq check.
echo "Checking git"
if [ ! -x "$(command -v git)" ]; then
	echo "git not found!! Exiting!!"
	exit 1
else
	git_ver_str="$(git --version | awk '{print $3}')"
	
	# Ubuntu can use PPA fresh git. So, let's use it.
	if ! grep -q 'ubuntu' /etc/os-release; then
		version_greater_equal "${git_ver_str}" "${git_minimal}" || install_git_ubuntu
	else
		version_greater_equal "${git_ver_str}" "${git_minimal}" || install_git
	fi
fi

echo "Checking Emacs... obviously."
if [ ! -x "$(command -v emacs)" ]; then
	echo "Emacs not found!! Exiting!"
	exit 1
else
	emacs_ver_str="$(emacs --version | awk 'NR==1{print $3}')"
	echo ${emacs_ver_str}
	version_greater_equal "${emacs_ver_str}" "27.1" || die "emacs version 27.1+ is needed! Better use 29.4+ or native-comp version."
fi

echo "Checking ripgrep"
if [ ! -x "$(command -v rg)" ]; then
	echo "ripgrep not found!! Exiting!"
	exit 1
else
	rg_ver_str="$(rg --version | awk 'NR==1{print $2}')"
	version_greater_equal "${rg_ver_str}" "${ripgrep_min_ver}" || die "ripgrep version ${ripgrep_min_ver}+ is needed!"
fi

echo "Checking find or fd"
if [ ! -x "$(command -v find)" ]; then
	echo "Gnu find not found in the system! Really!? What's going on!??"
	exit 1
fi

echo "Checking fd-find (Optional)"
if [ ! -x "$(command -v fd)" ]; then
	echo "fd-find not found in the system but it's not the end of the world!"
else
	fd_ver_str="$(fd --version | awk 'NR==1{print $2}')"
	version_greater_equal "${fd_ver_str}" "${fd_min_ver}" || die "fd-find was found but we need ${fd_min_ver}+!! Update or remove it!"
fi

if [ -d "$HOME/.emacs.d" ] || [ -e "$HOME/.emacs" ] || [ -L "$HOME/.emacs.d" ]; then
	if [ -d "$HOME/.doom.d" ] && [ -f "$HOME/.emacs.d/bin/doom" ]; then
		echo "Looks like we already have Doomemacs on the system!"
	else
		echo "Wiping out previous emacs settings."
		rm -rf "$HOME/.emacs"
		rm -rf "$HOME/.emacs.d"
    rm -rf "$HOME/.doom.d"
		rm -rf "$HOME/.spacemacs"
    rm -rf "$HOME/.local/share/doom"
	fi
fi

echo "Let's install Doomemacs!!"
git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.emacs.d" && "$HOME/.emacs.d/bin/doom" -! install || die 'Doom install failed!'

echo "Ok, updating the doom.d setting files!!"
rm -rvf "${HOME}"/.doom.d/*
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/init.el" "${HOME}/.doom.d/init.el" || true
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/config.el" "${HOME}/.doom.d/config.el" || true
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/packages.el" "${HOME}/.doom.d/packages.el" || true
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/config.org" "${HOME}/.doom.d/config.org" || true
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/aliases" "${HOME}/.doom.d/aliases" || true
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/profile" "${HOME}/.doom.d/profile" || true

echo "Re-compiling doomemacs with the updated setting files!!"
"$HOME/.emacs.d/bin/doom" -! sync -u

echo ""
echo "Doomemacs installed and Updated!!"
echo "Have fun!!"
echo ""
