#!/bin/bash -e

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

version_greater_equal()
{
	printf '%s\n%s\n' "$2" "$1" | sort -V -C
}

die()
{
	printf 'ERROR: %s\n' "$1"
	exit -1
}

# Doing some prereq check.
echo "Checking git"
if [ ! -x "$(command -v git)" ]; then
	echo "git not found!! Exiting!"
	exit 1
else
	git_ver_stdout=`echo $(git --version)`
	gvs_arr=($git_ver_stdout)
	git_ver_str="${gvs_arr[2]}"
	version_greater_equal "${git_ver_str}" "2.23.0" || die "git version 2.23+ is needed!"
fi

echo "Checking Emacs... obviously."
if [ ! -x "$(command -v emacs)" ]; then
	echo "Emacs not found!! Exiting!"
	exit 1
else
	emacs_ver_stdout=`echo $(emacs --version)`
	evs_arr=($emacs_ver_stdout)
	emacs_ver_str="${evs_arr[2]}"
	version_greater_equal "${emacs_ver_str}" "26.3" || die "emacs version 26.3+ is needed! Better use 27.1 or native-comp version."
fi

echo "Checking ripgrep"
if [ ! -x "$(command -v rg)" ]; then
	echo "ripgrep not found!! Exiting!"
	exit 1
else
	rg_ver_stdout=`echo $(rg --version)`
	rv_arr=( $rg_ver_stdout )
	rg_ver_str="${rv_arr[1]}"
	version_greater_equal "${rg_ver_str}" "11.0" || die "ripgrep version 11.0+ is needed!"
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
	fd_ver_stdout=`echo $(fd --version)`
	fv_arr=($fd_ver_stdout)
	fd_ver_str="${fv_arr[1]}"
	version_greater_equal "${fd_ver_str}" "7.3.0" || die "fd-find was found but we need 7.3.0+!! Update or remove it!"
fi

if [ -d "$HOME/.emacs.d" ] || [ -e "$HOME/.emacs" ] || [ -L "$HOME/.emacs.d" ]; then
	if [ -d "$HOME/.doom.d" ] && [ -f "$HOME/.emacs.d/bin/doom" ]; then
		echo "Looks like we already have Doomemacs on the system!"
	else
		echo "Wiping out previous emacs settings."
		rm -rf "$HOME/.emacs"
		rm -rf "$HOME/.emacs.d"

		echo "Let's install Doomemacs!!"
		git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
		"$HOME/.emacs.d/bin/doom" --yes install
	fi
fi

echo "Ok, updating the doom.d setting files!!"
# cp -vfr $SCRIPTPATH/../dotfiles/doom.d/*.el $HOME/.doom.d/
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/init.el" "${HOME}/.doom.d/init.el" || true
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/config.el" "${HOME}/.doom.d/config.el" || true 
ln -sv "${SCRIPTPATH}/../dotfiles/doom.d/packages.el" "${HOME}/.doom.d/packages.el" || true

echo "Re-compiling doomemacs with the updated setting files!!"
"$HOME/.emacs.d/bin/doom" --yes sync
"$HOME/.emacs.d/bin/doom" --yes upgrade

echo ""
echo "Doomemacs installed and Updated!!"
echo "Have fun!!"
