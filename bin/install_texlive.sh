#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
PROFILE_DEFAULT="${SCRIPTPATH}/texlive_inst.profile"
INST_SRC_DEFAULT="/tmp/texlive_install"

DESTDIR="$HOME/.texlive"

TEXLIVE_YEAR='2021'
TEXLIVE_ISO_NAME="texlive${TEXLIVE_YEAR}.iso"
TEXLIVE_ISO_URL="https://mirror.ctan.org/systems/texlive/Images/${TEXLIVE_ISO_NAME}"
TEXLIVE_ISO_MD5_URL="https://mirror.ctan.org/systems/texlive/Images/${TEXLIVE_ISO_NAME}.iso.md5"

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
    printf '\t-in,--image-network [iso image file url(optional)]\n'
	printf '\n'
}

# Checking prerequisites
[ ! -x "$(command -v perl)" ] && die 'Oh boy, we need Perl!\n'
[ ! -x "$(command -v wget)" ] && die 'wget is needed!\n'

# Argument parsing
INST_SRC=''
IMG_FILE=''
IMG_FILE_URL=''
PROFILE=''
netflag=''
iflag=''
inflag=''
pflag=''
fclean=''
while (($# > 0)); do
	case "$1" in
	-h | --help)
		usage
		exit 0
		;;
	-*)
		if [ "$1" == "--profile" ] || [ "$1" == "-p" ]; then
			pflag='-p'
		fi
		
		if [ "$1" == "-n" ] || [ "$1" == "--network" ]; then
			netflag='-n'
		fi

		if [ "$1" == "-i" ] || [ "$1" == "--image" ]; then
			iflag='-i'
		fi
		
		if [ "$1" == "-in" ] || [ "$1" == "--image-network" ]; then
		    inflag='-in'
		fi

		if [ "$1" == '--clean' ]; then
			fclean='--clean'
		fi

		shift
		;;
	*)
		if [ ! -z "$pflag" ]; then
			PROFILE="$1"
		fi

		if [ ! -z "$netflag" ]; then
			INST_SRC="$1"
		fi

		if [ ! -z "$iflag" ]; then
			IMG_FILE="$1"
		fi
		
		if [ ! -z "$inflag" ]; then
		    IMG_FILE_URL="$1"
		fi

		shift
		;;
	esac
done

[ -n "$inflag" ] && [ -z "${IMG_FILE_URL}" ] && IMG_FILE_URL="${TEXLIVE_ISO_URL}"

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
if [ ! -z "$netflag" ]; then
	wget "$INST_SRC" -O "$TMP_TARBALL"
	printf 'Extracting archive...\n'
	if [ ! -d "$INST_SRC_DEFAULT" ]; then
		tar xf "$TMP_TARBALL" -C "$INST_SRC_DEFAULT"
	else
		printf 'Working directory %s already exists! Trying to use it!\n' "$INST_SRC_DEFAULT"
		printf 'If installation fails, remove it and extract it again!\n'
	fi
fi

# Install from image
extract_image () {
    [ ! -f "$1" ] && die "Given iso image file does not exist!!"
    
    local TMP_ISO_DIR="/tmp/tmp_iso"
    
    if [ -f "$TMP_ISO_DIR/install-tl" ]; then
        printf 'texlive seems to be already extracted!!\n'
        return 0
    fi
    
    if [ -x "$(command -v 7z)" ]; then
        # We can use 7z to extract ISO directly!!
        [ ! -f "$INST_SRC_DEFAULT" ] && mkdir -p "$INST_SRC_DEFAULT"
        7z x "$1" -o"$INST_SRC_DEFAULT/" -r -y && chmod +x "$INST_SRC_DEFAULT/install-tl"
    else
        # In case we don't have 7z on the system...
        printf 'Trying to mount the image  file...\n'
        
	    mkdir -p "$TMP_ISO_DIR" && sudo mount -t iso9660 -o loop "$1" "$TMP_ISO_DIR"
	    [ ! -d "$INST_SRC_DEFAULT" ] && mkdir -p "$INST_SRC_DEFAULT"
	    printf 'Copying ISO file contents to temporary installation directory.\n'
	    if [ -x "$(command -v rsync)" ]; then
	        rsync -azvh --progress "$TMP_ISO_DIR"/* "$INST_SRC_DEFAULT/" && \
	        sudo umount "$TMP_ISO_DIR" && rm -rf "$TMP_ISO_DIR"
	    else
	        cp -pfr "$TMP_ISO_DIR"/* "$INST_SRC_DEFAULT/" && sudo umount "$TMP_ISO_DIR" && rm -rf "$TMP_ISO_DIR"
	    fi
	fi
	return 0
}

# If image file is given...
if [ ! -z "${IMG_FILE}" ]; then
	printf 'Using image file at: %s\n' "$IMG_FILE"
	extract_image "${IMG_FILE}"
fi

# If image file is given as network address
TMP_ISO_FILE="/tmp/${TEXLIVE_ISO_NAME}"
if [ ! -z "${IMG_FILE_URL}" ]; then
    if [ ! -f "${TMP_ISO_FILE}" ]; then
        printf 'Downloading image file from ...\n%s\n' "${IMG_FILE_URL}"
        wget "${IMG_FILE_URL}" -O "${TMP_ISO_FILE}" || die "Downloading Texlive ISO file failed!!"
    else
        printf 'Already downloaded iso found! Checking it!\n'
        wget "$TEXLIVE_ISO_MD5_URL" -O /tmp/texlive.iso.md5
        if [ -x "$(command -v md5sum)" ]; then
            md5result="$(cd /tmp && md5sum -c /tmp/texlive.iso.md5 | grep "OK")"
            if [ "$md5result" = "OK" ]; then
                printf 'MD5 Check up holds!\n'
            else
                printf 'MD5 Check up does not holds! Please Run the script again!\n'
                rm -rf "${TMP_ISO_FILE}"
            fi
        fi
    fi
    extract_image "${TMP_ISO_FILE}"
fi

if [ -z "$iflag" ] && [ -z "$netflag" ] && [ -z "$inflag" ]; then
	[ ! -f "$INST_SRC_DEFAULT/install-tl" ] && die "Local install-tl and packages are not found!!"
	printf 'Using local installation source at: %s\n' "$INST_SRC_DEFAULT"
	INST_SRC="$INST_SRC_DEFAULT"
fi


# Let's install the packages into $HOME/.texlive !!
SRC_DIR="$INST_SRC_DEFAULT"
if [ ! -z "$(grep -i 'rhel' /etc/os-release)" ]; then
    printf 'It looks like current distro is RHEL based one.\n'
    printf 'Checking if required modules installed...\n'
    if [ -x "$(command -v dnf)" ]; then
        if [ -z "$(dnf list installed | grep -i 'perl-Digest-MD5')" ]; then
            printf 'Installing some perl modules to use Texlive installer.\n'
            sudo dnf install -y perl-Digest-MD5
        fi
    else
        if [ -z "$(yum list installed | grep -i 'perl-Digest-MD5')" ]; then
            printf 'Installing some perl modules to use Texlive installer.\n'
            sudo yum install -y perl-Digest-MD5
        fi
    fi
fi
cd "$SRC_DIR" && ./install-tl --profile "$PROFILE" || die "Something went wrong during installation!!"

# Making a symbolic link for the newest Texlive
# kinda brute force approach... change here later.
texlive_years=(2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2035)
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
[ -n "$fclean" ] && rm -rf "$SRC_DIR" "$TMP_TARBALL" "${TMP_ISO_FILE}"

printf 'Jobs finished!\n'
