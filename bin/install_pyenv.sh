#!/bin/bash -e

PY_VER='install-latest'
if [ ! -z "$1" ]; then
	printf 'Selecting python version as %s\n' "$1"
	PY_VER="$1"
else
	printf 'Selecting latest CPython\n'
fi

die() {
	printf '%s\n' "$1"
	exit 1
}

PYTHON_PKGS=(
	"pip"
	"autopep8" "xlrd" "xlsxwriter" "sphinx"
	"pylint" "pyparsing" "pyopengl"
	"numpy" "scipy" "matplotlib" "pandas" "nose"
	"sympy" "pyinstaller" "jupyter" "nose" "virtualenv"
)

COMPILE_OPTS=(
	"--enable-shared"
	"--enable-ipv6"
	"--enable-unicode=ucs4"
	"--with-threads"
	"--with-valgrind"
	"--with-ensurepip=yes"
	"--with-system-ffi"
	"--with-system-expat"
	"--enable-optimizations"
)

C_FLAGS="-O3 -fomit-frame-pointer -march=native -pipe"

COMP_OPTS_STR=$(
	IFS=' '
	echo "${COMPILE_OPTS[*]}"
)

# Checking out their git repository to my own pyenv dir...
#
# Their suggestion was ~/.pyenv
#
PYENV_DIR="$HOME/.pyenv"
[ -d "$PYENV_DIR" ] && rm -rf "$PYENV_DIR"
[ ! -x "$(command -v git)" ] && die 'git not found!! Exiting!!'
[ ! -x "$(command -v curl)" ] && die 'curl not found!! Exiting!!'

# Finally installing it!
if [ ! -d "$PYENV_DIR" ]; then
	printf 'Cloning pyenv into %s ...\n' "$PYENV_DIR"
	git clone 'https://github.com/pyenv/pyenv.git' "$PYENV_DIR" || die "Failed to clone pyenv!! Exiting!!"
fi

# Grabbing Python build prereqs.
# TODO Implement yet another distro detection stuff here...
# Or.. should we at least make a program?

# Let's install some default python installations!!
export PYENV_ROOT="$PYENV_DIR"
export PATH="$PYENV_DIR/bin:$PATH"
eval "$(pyenv init -)"
INSTALL_SUCCESS='true'
if [ ! -f "$PYENV_ROOT/shims/python" ]; then
	git clone 'https://github.com/momo-lab/pyenv-install-latest.git' "$PYENV_ROOT/plugins/pyenv-install-latest" || die "pyenv-install-latest cloning failed!!"
	if [ "$PY_VER" = 'install-latest' ]; then
		env PYTHON_CONFIGURE_OPTS="$COMP_OPTS_STR" CFLAGS="$C_FLAGS" pyenv install-latest || INSTALL_SUCCESS='false'
		[ "$INSTALL_SUCCESS" = 'true' ] && pyenv global "$(pyenv install-latest --print)"
	else
		env PYTHON_CONFIGURE_OPTS="$COMP_OPTS_STR" CFLAGS="$C_FLAGS" pyenv install "$PY_VER" || INSTALL_SUCCESS='false'
		[ "$INSTALL_SUCCESS" = 'true' ] && pyenv global "$PY_VER"
	fi
fi
PY_PYENV="$PYENV_DIR/shims/python"
if [ "$INSTALL_SUCCESS" = 'true' ]; then
	# Let's ensure pip installed!
	curl -O 'https://bootstrap.pypa.io/get-pip.py' || die "Getting get-pip.py failed! Exiting!!"
	"$PY_PYENV" ./get-pip.py || die "Failed to install pip with get-pip.py!! Oops!!"
	PIP="$PYENV_DIR/shims/pip"
	rm -rf ./get-pip.py

	for pypi in "${PYTHON_PKGS[@]}"; do
		"$PIP" install -U "$pypi" || true
	done
fi

# Warning messages
if [ "$INSTALL_SUCCESS" = 'false' ]; then
	printf '\nOops, it seems we failed to install default version of python.\n\n'
	printf 'Install build requirements then try again yourself later!\n'
	printf '\nHere are some build dependency information!!\n\n'
	printf '\nMac OS X\nbrew install openssl readline sqlite3 xz zlib\n\n'
	printf '\nUbuntu Family\nsudo apt update && sudo apt install -y make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev\n\n'
	printf '\nFedora Family (before 21)\nsudo yum install gcc zlib-devel bzip2 bzip2-devel readline-devel sqlite sqlite-devel openssl-devel tk-devel libffi-devel\n\n'
	printf '\nFedora Family (Modern)\nsudo dnf -y install make gcc zlib-devel bzip2 bzip2-devel readline-devel sqlite sqlite-devel openssl-devel tk-devel libffi-devel\n\n'
	printf '\nopenSUSE\nsudo zypper install gcc automake bzip2 libbz2-devel xz xz-devel openssl-devel ncurses-devel readline-devel zlib-devel tk-devel libffi-devel sqlite3-devel xz\n\n'
	printf '\nArch Linux\nsudo pacman -Syyu base-devel openssl zlib\n\n'
	printf '\nSolus\nsudo eopkg it -c system.devel && sudo eopkg install git gcc make zlib-devel bzip2-devel readline-devel sqlite3-devel openssl-devel tk-devel\n\n'
	printf 'Then, just put: pyenv install <python_ver>\n'
fi

# Closing up comments...
printf '\n\nMake sure those messages added into your shell init files.\n'
printf 'PYENV_ROOT=%s\n' "$PYENV_DIR"
printf 'PATH=%s/bin:$PATH\n' "$PYENV_DIR"
printf 'eval $(pyenv init -) # if bash or zsh\n'
printf 'eval $(pyenv init --path) # To add pyenv to path!\n'
printf '\nThen you are good to go after next session!\n\n'
