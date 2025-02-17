#!/bin/sh

echo "This script is deprecated!! Running this script may introduce library hell in your home directory."
echo "Are you still going to continue? [y/N]"
read answer
if [ "$answer" != "${answer#[Yy]}" ] ;then
    echo "Ok, continueing the operatin... you have been warned!!"
else
    echo "Canceling the script operation"
	return 1
fi

HOMEBREW=$(command -v brew)

if [[ -z $HOMEBREW ]]; then
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
	echo "Homebrew found at $HOMEBREW"
fi

PKGS=("wget" "python3" "gnuplot" "git" "nano" "gcc" "ngspice" "lua" "gdb" "gdbgui" "xz" "youtube-dl" "ffmpeg" "macvim" "p7zip" "bison" "flex" "doxygen" "mercurial" "node" "autoconf" "automake" "cmake" "cgdb" "emacs" "qt" "perl" "pcre2" "rbenv" "scipy" "numpy" "openblas" "subversion" "sqlite" "speex" "unrar" "unzip" "go" "rust" "ghc" "zeromq")

CASK_PKGS=("macvim" "emacs" "cmake")

RUBY_GEMS=("json" "hjson" "rails" "open3" "rsense")

INSTALLED_PKGS=( $(brew list) $(brew list --cask) )

FFMPEG_OPTIONS=( "--with-fdk-aac" "--with-game-music-emu" "--with-libbluray" "--with-openjpeg" "--with-rav1e" "--with-openh264" "--with-libssh" "--with-libxml2" "--with-openssl" "--with-srt" "--with-tesseract" "--with-xvid" "--with-zeromq" "--with-zimg" "--with-speex" "--HEAD" )
printf -v FFMPEG_OPTIONS_STR "%s " "${FFMPEG_OPTIONS[@]}"

PYTHON3_PKGS=("numpy" "scipy" "matplotlib" "spyder" "jupyter" "ipython" "sphinx" "qtconsole" "pyinstaller" "pexpect" "sphinx" "pyopengl" "pandas" "sympy" "ipywidgets" "proio" "xlrd" "xlsxwriter" "pylint" "pyparsing" "autopep8" "pip-manager")

# Now install all the packages we need!!
for PKG in ${PKGS[@]};
do
	if [[ ! " ${INSTALLED_PKGS[@]} " =~ " ${PKG} " ]]; then
		# Something specific for ffmpeg.. we need more codecs than
		# GPL limited one.
		if [[ $PKG == "ffmpeg" ]]; then
			echo "Installing FFMPEG from tap!"
			brew tap homebrew-ffmpeg/ffmpeg
			brew install homebrew-ffmpeg/ffmpeg/ffmpeg $FFMPEG_OPTIONS_STR
		elif [[ $PKG == "python3" ]]; then
			CHK_PY3=$(brew list | grep python@3)
			if [[ -z $CHK_PY3 ]]; then
				echo "Installing Latest Python3"
				brew install $PKG
			fi
		elif [[ " ${CASK_PKGS[@]} " =~ " ${PKG} "  ]]; then
			echo "installing $PKG from cask"
			brew cask install $PKG
		else
			echo "Installing $PKG"
			brew install $PKG
		fi
	fi
done

# Ruby packages
RGEM=$HOME/.rbenv/shims/gem
if ! type $RGEM > /dev/null 2>&1; then
	echo "Ruby installation has been changed to use rbenv."
	echo ">> rbenv install <desired ruby version>"
	echo "to install your preferred ruby"
else
	echo "Installing some gems for ruby"
	for _GEM_ in ${RUBY_GEMS[@]};
	do
		$RGEM install $_GEM_
	done
fi

# Python package installation
PIP3=/usr/local/bin/pip3
if type $PIP3 > /dev/null 2>&1; then
	echo "Installing some python3 packages!"
	for _PY3PKG_ in ${PYTHON3_PKGS[@]};
	do
		$PIP3 install -U $_PY3PKG_
	done
fi

echo
echo *********************
echo * Jobs finished!!!! *
echo *********************
echo
