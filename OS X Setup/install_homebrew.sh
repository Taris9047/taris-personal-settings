#!/bin/sh

HOMEBREW=$(which brew)

if [[ -z $HOMEBREW ]]; then
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
	echo "Homebrew found at $HOMEBREW"
fi

PKGS=("wget" "python3" "gnuplot" "git" "nano" "gcc" "ngspice" "lua" "gdb" "gdbgui" "xz" "youtube-dl" "ffmpeg" "nano" "macvim" "p7zip" "bison" "flex" "doxygen" "mercurial" "node" "autoconf" "automake" "cmake" "cgdb" "emacs" "pypy3" "qt" "perl" "pcre2" "ruby" "scipy" "numpy" "openblas" "subversion" "sqlite" "speex" "unrar" "unzip" "go")

INSTALLED_PKGS=( $(brew list) )

FFMPEG_OPTIONS=( "--with-fdk-aac" "--with-game-music-emu" "--with-libbluray" "--with-openjpeg" "--with-rav1e" "--with-openh264" "--with-libssh" "--with-libxml2" "--with-openssl" "--with-srt" "--with-tesseract" "--with-xvid" "--with-zeromq" "--with-zimg" "--with-speex" "--HEAD" )
printf -v FFMPEG_OPTIONS_STR "%s " "${FFMPEG_OPTIONS[@]}"

#echo "First, updating homebrew itself!!"
#brew upgrade

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
		else
			echo "Installing $PKG"
			brew install $PKG
		fi
	fi
done

echo Jobs finished!!!