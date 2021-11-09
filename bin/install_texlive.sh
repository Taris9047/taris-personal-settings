#!/bin/bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
PROFILE_DEFAULT="${SCRIPTPATH}/texlive_inst.profile"
INST_SRC_DEFAULT="$HOME/.texlive_install"

DESTDIR="$HOME/.texlive"

# Some crappy utilities for me
die() {
	printf 'ERROR: %s\n' "$1"
	exit 1
}

# Some help message
usage() {
	printf '%s [options]\n' "$0"
	printf '[options]:\n'
	printf '\t-n,--network [Network Address or install_tl]\n'
	printf '\t-p,--profile [Profile Location]\n'
  printf '\t-i,--image [iso image file location]\n'
	printf '\n'
}

# Checking prerequisites
if [ ! -x "$(command -v perl)" ]; then
	printf 'Oh boy, we need Perl!\n'
	exit 1
fi
if [ ! -x "$(command -v wget)" ]; then
	printf 'wget is needed!\n'
	exit 1
fi

# Argument parsing
INST_SRC=''
IMG_FILE=''
PROFILE=''
netflag=''
iflag=''
pflag=''
fclean=''
while (($# > 0)); do
	case "$1" in
	-h | --help)
		usage
		exit 0
		;;
	-*)
		if [[ "$1" == "--profile" || "$1" == "-p" ]]; then
			pflag='-p'
		fi
		
		if [[ "$1" == "-n" || "$1" == "--network" ]]; then
			netflag='-n'
		fi

		if [[ "$1" == "-i" || "$1" == "--image" ]]; then
			iflag='-i'
		fi

		if [[ "$1" == '--clean' ]]; then
			fclean='--clean'
		fi

		shift
		;;
	*)
		if [ ! -z "$pflag" ]; then
			[ -z "$PROFILE" ] && PROFILE="$1"
		fi

		if [ ! -z "$netflag" ]; then
			[ -z "$INST_SRC" ] && INST_SRC="$1"
		fi

		if [ ! -z "$iflag" ]; then
			[ -z "$IMG_FILE" ] && IMG_FILE="$1"
		fi

		shift
		;;
	esac
done

if [ -z "$PROFILE" ]; then
	PROFILE="$PROFILE_DEFAULT"
fi

# Check if those options are sane
if [ -f "$PROFILE" ]; then
	printf 'Reading installation profile from %s\n' "$PROFILE"
else
	die "O crap, we cannot find the profile file: $PROFILE anywhere!"
fi



##### Ok, let's actually run the installation! #####

SRC_DIR=''
TMP_TARBALL='./texlive_install_pkgs.tar.xz'

# in terms of network downloading..
if [[ ! -z "$netflag" ]]; then
	wget "$INST_SRC" -O "$TMP_TARBALL"
	printf 'Extracting archive...\n'
	if [ ! -d "$INST_SRC_DEFAULT" ]; then
		tar xf "$TMP_TARBALL" -C "$INST_SRC_DEFAULT"
	else
		printf 'Working directory %s already exists! Trying to use it!\n' "$INST_SRC_DEFAULT"
		printf 'If installation fails, remove it and extract it again!\n'
	fi
fi

# If image file is given...
if [ ! -z "$IMG_FILE" ]; then
	printf 'Using image file at: %s\n' "$IMG_FILE"
	printf 'Trying to mount the image  file...\n'
	mkdir -p ./tmp_iso && sudo mount -t iso9660 -o loop "$IMG_FILE" ./tmp_iso
	[ ! -d "$INST_SRC_DEFAULT" ] && mkdir -p "$INST_SRC_DEFAULT"
	cp -pfr ./tmp_iso/* "$INST_SRC_DEFAULT/" && sudo umount ./tmp_iso && rm -rf ./tmp_iso
fi

if [ -z "$iflag" ] && [ -z "$netflag" ]; then
	[ ! -f "$INST_SRC_DEFAULT/install-tl" ] && die "Local install-tl and packages are not found!!"
	printf 'Using local installation source at: %s\n' "$INST_SRC_DEFAULT"
	INST_SRC="$INST_SRC_DEFAULT"
fi


# Let's install the packages into $HOME/.texlive !!
SRC_DIR="$INST_SRC_DEFAULT"
cd "$SRC_DIR" && ./install-tl --profile "$PROFILE" || die "Something went wrong during installation!!"

# Making a symbolic link for the newest Texlive
# kinda brute force approach... change here later.
texlive_years=(2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030)
texlive_base_path="$HOME/.texlive"
texlive_year='current'
texlive_arch='x86_64-linux'
texlive_bin_dir="$texlive_base_path/$texlive_year/bin/$texlive_arch/"
for yr in ${texlive_years[@]}; do
    if [ ! -d "$texlive_base_path/$texlive_year" ] && [ -d "$texlive_base_path/$yr" ]; then
        ln -sf "$texlive_base_path/$yr" "$texlive_base_path/current"
    fi
done

# Handling fonts
kpsewhich_cmd="$HOME/.texlive/current/bin/x86_64-linux/kpsewhich"
Fonts_dir="$HOME/.local/share/fonts/conf.d"
[ ! -d "$Fonts_dir" ] && mkdir -p "$Fonts_dir"
cp -f $($kpsewhich_cmd -var-value TEXMFSYSVAR)/fonts/conf/texlive-fontconfig.conf \
	"$Fonts_dir/09-texlive.conf"

# Now we're done
[ -x "$(command -v fc-cache)" ] && fc-cache -fs

# Cleaning up... 
[ -n "$fclean" ] && rm -rf "$SRC_DIR" "$TMP_TARBALL"

printf 'Jobs finished!\n'
