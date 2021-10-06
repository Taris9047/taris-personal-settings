#!/bin/bash

# Referenced:
# https://github.com/markus-perl/ffmpeg-build-script

VERSION=1.25
CWD=$(pwd -P)
PACKAGES=$CWD/packages
WORKSPACE=$CWD/workspace
CC="$(command -v clang)"
CXX="$(command -v clang++)"

# Fallback compilers
if [ ! -x "$(command -v clang)" ]; then
	echo "Oops, clang can't be found!! Falling back to system GCC."
	CC="$(command -v gcc)"
	CXX="$(command -v g++)"
fi

# Setting up build environments
LDFLAGS_Z="-Wl,-rpath=${WORKSPACE}/lib"
LDFLAGS="${LDFLAGS_Z}"
LDEXEFLAGS=""
EXTRALIBS="-ldl -lpthread -lm -lz"
case "$CC" in
*"clang")
	CFLAGS="-I${WORKSPACE}/include -O3 -march=native -pipe -fomit-frame-pointer -fPIC -fPIE"
	;;
*"gcc")
	CFLAGS="-I${WORKSPACE}/include -O3 -march=native -pipe -fomit-frame-pointer -fno-semantic-interposition -fPIC -fPIE"
	;;
*) ;;
esac

CXXFLAGS=$CFLAGS

COMPILER_SET="CC=\"$CC\" CXX=\"$CXX\" CFLAGS=\"$CFLAGS\" CXXFLAGS=\"$CXXFLAGS\" LDFLAGS=\"$LDFLAGS\" "
COMPILER_SET_Z="CC=\"$CC\" CXX=\"$CXX\" CFLAGS=\"$CFLAGS\" CXXFLAGS=\"$CXXFLAGS\" LDFLAGS=\"$LDFLAGS_Z\" "

NVCC_VER_THRSH="8.0.13"

CONFIGURE_OPTIONS=()

INSTALL_FOLDER="$HOME/.local/bin"
if [[ "$OSTYPE" == "darwin"* ]]; then
	INSTALL_FOLDER="/usr/local/bin"
fi

# Setting up pkgconfig stuffs
export PATH="${WORKSPACE}/bin:$PATH"
PKG_CONFIG_PATH="${WORKSPACE}/lib/pkgconfig"
PKG_CONFIG_PATH+="/usr/local/lib/x86_64-linux-gnu/pkgconfig:/usr/local/lib/pkgconfig"
PKG_CONFIG_PATH+=":/usr/local/share/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/lib64/pkgconfig"
export PKG_CONFIG_PATH

COMPILER_SET+=" PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\""


PYTHON=''
if [ -x "$(command -v python3)" ]; then
	PYTHON="$(command -v python3)"
elif [ -x "$(command -v python2)" ]; then
	PYTHON="$(command -v python2)"
else
	PYTHON='i_have_no_python_booooz'
fi

if [ ! -x "$(command -v $CC)" ]; then
	echo "$CC"
	echo "No compiler found!!"
	exit 1
fi

if [ ! -x "$(command -v make)" ]; then
	echo "No make in this system!!"
	exit 1
fi

if [ ! -x "$(command -v curl)" ]; then
	echo "No curl found in this system!!"
	exit 1
fi

if [ ! -x "$PYTHON" ]; then
	echo "No Python found! Lv2 won't be available!"
fi

# Speed up the process
# Env Var NUMJOBS overrides automatic detection
if [[ -n "$NUMJOBS" ]]; then
	MJOBS="$NUMJOBS"
elif [[ -f /proc/cpuinfo ]]; then
	MJOBS=$(grep -c processor /proc/cpuinfo)
elif [[ "$OSTYPE" == "darwin"* ]]; then
	MJOBS=$(sysctl -n machdep.cpu.thread_count)
	CONFIGURE_OPTIONS+=("--enable-videotoolbox")
else
	MJOBS=4
fi

# Versions
pkgconfig_ver="0.29.2"
yasm_ver="1.3.0"
nasm_ver="2.15.05"
zlib_ver="1.2.11"
m4_ver="1.4.19"
autoconf_ver="2.71"
automake_ver="1.16.4"
libtool_ver="2.4.6"
openssl_ver="1_1_1l"
cmake_ver="3.21.2"
git_ver="2.33.0"
dav1d_ver="0.9.2"
x264_ver="5db6aa6cab1b146e07b60cc1736a01f21da01154"
x265_ver="3.5"
libvpx_ver="1.10.0"
xvidcore_ver="1.3.7"
vid_stab_ver="1.1.0"
zimg_ver="3.0.3"
lv2_ver="1.18.2"
waflib_ver="b600c928b221a001faeab7bd92786d0b25714bc8"
serd_ver="0.30.10"
pcre_ver="8.44"
sord_ver="0.16.8"
sratom_ver="0.6.8"
lilv_ver="0.24.12"
opencore_ver="0.1.5"
lame_ver="3.100"
opus_ver="1.3.1"
libogg_ver="1.3.3"
libvorbis_ver="1.3.6"
libtheora_ver="1.1.1"
fdk_aac_ver="2.0.2"
libtiff_ver="4.3.0"
libpng_ver="1.6.37"
libwebp_ver="1.2.0"
libsdl_ver="2.0.14"
srt_ver="1.4.3"
nvcodec_ver="11.1.5.0"
amf_ver="1.4.21.0"

# Functions...
make_dir() {
	if [ ! -d "$1" ]; then
		if ! mkdir "$1"; then
			printf "\n Failed to create dir %s" "$1"
			exit 1
		fi
	fi
}

remove_dir() {
	if [ -d "$1" ]; then
		rm -rf "$1"
	else
	    echo "$1 does not exist! Passing!"
	fi
}

download() {
	# download url [filename[dirname]]

	DOWNLOAD_PATH="$PACKAGES"
	DOWNLOAD_FILE="${2:-"${1##*/}"}"

	if [[ "$DOWNLOAD_FILE" =~ "tar." ]]; then
		TARGETDIR="${DOWNLOAD_FILE%.*}"
		TARGETDIR="${3:-"${TARGETDIR%.*}"}"
	else
		TARGETDIR="${3:-"${DOWNLOAD_FILE%.*}"}"
	fi

	if [ ! -f "$DOWNLOAD_PATH/$DOWNLOAD_FILE" ]; then
		echo "Downloading $1 as $DOWNLOAD_FILE"
		curl -L --silent -o "$DOWNLOAD_PATH/$DOWNLOAD_FILE" "$1"

		EXITCODE=$?
		if [ "$EXITCODE" -ne 0 ]; then
			echo ""
			echo "Failed to download $1. Exitcode $EXITCODE. Retrying in 10 seconds"
			sleep 10
			curl -L --silent -o "$DOWNLOAD_PATH/$DOWNLOAD_FILE" "$1"
		fi

		EXITCODE=$?
		if [ "$EXITCODE" -ne 0 ]; then
			echo ""
			echo "Failed to download $1. Exitcode $EXITCODE"
			exit 1
		fi

		echo "... Done"
	else
		echo "$DOWNLOAD_FILE has already downloaded."
	fi

	make_dir "$DOWNLOAD_PATH/$TARGETDIR"

	if [ -n "$3" ]; then
		if ! tar -xvf "$DOWNLOAD_PATH/$DOWNLOAD_FILE" -C "$DOWNLOAD_PATH/$TARGETDIR" 2>/dev/null >/dev/null; then
			echo "Failed to extract $DOWNLOAD_FILE"
			exit 1
		fi
	else
		if ! tar -xvf "$DOWNLOAD_PATH/$DOWNLOAD_FILE" -C "$DOWNLOAD_PATH/$TARGETDIR" --strip-components 1 2>/dev/null >/dev/null; then
			echo "Failed to extract $DOWNLOAD_FILE"
			exit 1
		fi
	fi

	echo "Extracted $DOWNLOAD_FILE"

	cd "$DOWNLOAD_PATH/$TARGETDIR" || (
		echo "Error has occurred."
		exit 1
	)
}

execute() {
	echo "$ $*"

	OUTPUT="$(eval "$@" 2>&1)"

	# shellcheck disable=SC2181
	if [ $? -ne 0 ]; then
		echo "$OUTPUT"
		echo ""
		echo "Failed to Execute $*" >&2
		exit 1
	fi
}

build() {
	echo ""
	echo "building $1"
	echo "======================="

	if [ -f "$PACKAGES/$1.done" ]; then
		echo "$1 already built. Remove $PACKAGES/$1.done lockfile to rebuild it."
		return 1
	fi

	return 0
}

command_exists() {
	if ! [[ -x $(command -v "$1") ]]; then
		return 1
	fi

	return 0
}

library_exists() {
	local result=0
	local output=$(pkg-config --exists --print-errors "$1" 2>&1 >/dev/null) || result=$?
	if [ ! "$result" = "0" ]; then
		return 1
	fi

	return 0
}

build_done() {
	touch "$PACKAGES"/"$1.done"
}

cleanup() {
	remove_dir "$PACKAGES"
	remove_dir "$WORKSPACE"
	echo "Cleanup done."
	echo ""
}

version() {
    echo "$0 $VERSION"
}

# Adopted from
#
# https://stackoverflow.com/a/4025065
#
# case $? in
#   0) op='=';;
#   1) op='>';;
#   2) op='<';;
# esac

vercomp() {
	if [[ "$1" == "$2" ]]; then
		return 0
	fi
	local IFS=.
	local i ver1=($1) ver2=($2)
	# fill empty fields in ver1 with zeros
	for ((i = ${#ver1[@]}; i < ${#ver2[@]}; i++)); do
		ver1[i]=0
	done
	for ((i = 0; i < ${#ver1[@]}; i++)); do
		if [[ -z ${ver2[i]} ]]; then
			# fill empty fields in ver2 with zeros
			ver2[i]=0
		fi
		if ((10#${ver1[i]} > 10#${ver2[i]})); then
			return 1
		fi
		if ((10#${ver1[i]} < 10#${ver2[i]})); then
			return 2
		fi
	done
	return 0
}

get_nvcc_ver() {
	if [ -v "$(command -v nvcc)" ]; then
		nvcc_ver="$(nvcc --version | grep -oP '^Cuda compilation.*release \K[0-9]+\.[0-9]+')"
		echo "$nvcc_ver"
	else
		echo "0.0.0"
	fi
}

nvcc_ver_chk() {
	vercomp $(get_nvcc_ver) $NVCC_VER_THRSH
	case $? in
	0) echo 'Pass' ;;
	1) echo 'Pass' ;;
	2) echo 'Fail' ;;
	esac
}

verify_binary_type() {
	if ! command_exists "file"; then
		return
	fi

	BINARY_TYPE=$(file "$WORKSPACE/bin/ffmpeg" | sed -n 's/^.*\:\ \(.*$\)/\1/p')
	echo ""
	case $BINARY_TYPE in
	"Mach-O 64-bit executable arm64")
		echo "Successfully built Apple Silicon (M1) for ${OSTYPE}: ${BINARY_TYPE}"
		;;
	*)
		echo "Successfully built binary for ${OSTYPE}: ${BINARY_TYPE}"
		;;
	esac
}

usage() {
    echo "Usage: $0"
    echo "   --build: start building process"
    echo "   --cleanup: remove all working dirs"
    echo "   --version: shows version info."
    echo "   --help: show this help"
    echo ""
}

bflag=''
cflag=''

while (($# > 0)); do
    case "$1" in
    -h | --help)
        usage
	    exit 0
	    ;;

	--version)
	    version
	    exit 0
	    ;;
	
    -*)
        if [[ "$1" == "--build" ]]; then
            bflag='-b'
        fi

        if [[ "$1" == "--full-static" ]]; then
        	if [[ "$OSTYPE" == "darwin"* ]]; then
        		echo "Error: A full static binary can only be build on Linux."
        		exit 1
        	fi
        	LDEXEFLAGS="-static"
	    fi
	    
	    if [[ "$1" == "--cleanup" ]]; then
	        cflag='-c'
	        cleanup
	    fi
	    
	    shift
	    ;;
	*)
	    usage
	    exit 1
	    ;;
    esac
done

if [ -z "$bflag" ]; then
    if [ -z "$cflag" ]; then
        usage
	    exit 1
	fi
	exit 0
fi

echo "ffmpeg-build-script v$VERSION"
echo "========================="
echo ""
echo "Compilers are..."
echo "C Compiler=${CC}"
echo "C++ Compiler=${CXX}"
echo ""

echo "Using $MJOBS make jobs simultaneously."

if [ -n "$LDEXEFLAGS" ]; then
  echo "Start the build in full static mode."
fi

make_dir "$PACKAGES"
make_dir "$WORKSPACE"

export PATH=${WORKSPACE}/bin:$PATH

if ! command_exists "make"; then
	echo "make not installed."
	exit 1
fi

if ! command_exists "g++"; then
	echo "g++ not installed."
	exit 1
fi

if ! command_exists "curl"; then
	echo "curl not installed."
	exit 1
fi

if ! command_exists "cmake"; then
	echo "cmake not installed."
	exit 1
fi

if [ -n "$LDEXEFLAGS" ]; then
	echo "Start the build in full static mode."
fi

## Base Libraries to build stuffs
if build "zlib"; then
	download "https://www.zlib.net/zlib-$zlib_ver.tar.gz" "zlib-$zlib_ver.tar.gz"
	cd "$PACKAGES"/zlib-$zlib_ver || exit
	execute env "$COMPILER_SET_Z" ./configure --static --prefix="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install
	build_done "zlib"
fi
LDFLAGS+=" -L/zlib/lib"

if build "pkg-config"; then
	download "https://pkgconfig.freedesktop.org/releases/pkg-config-$pkgconfig_ver.tar.gz" "pkg-config-$pkgconfig_ver.tar.gz"
	cd "$PACKAGES"/pkg-config-$pkgconfig_ver || exit
	execute env "$COMPILER_SET" ./configure --silent --prefix="${WORKSPACE}" --with-pc-path="${WORKSPACE}"/lib/pkgconfig --with-internal-glib --disable-host-tool
	execute make -j $MJOBS
	execute make install
	build_done "pkg-config"
fi

if build "yasm"; then
	download "https://www.tortall.net/projects/yasm/releases/yasm-$yasm_ver.tar.gz" "yasm-$yasm_ver.tar.gz"
	cd "$PACKAGES"/yasm-$yasm_ver || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install
	build_done "yasm"
fi

if build "nasm"; then
	download "https://www.nasm.us/pub/nasm/releasebuilds/$nasm_ver/nasm-$nasm_ver.tar.xz" "nasm-$nasm_ver.tar.xz"
	cd "$PACKAGES"/nasm-$nasm_ver || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	build_done "nasm"
fi

if build "m4"; then
  download "https://ftp.gnu.org/gnu/m4/m4-${m4_ver}.tar.gz" "m4-${m4_ver}.tar.gz"
  cd "$PACKAGES"/m4-${m4_ver} || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "m4"
fi

if build "autoconf"; then
  download "https://ftp.gnu.org/gnu/autoconf/autoconf-${autoconf_ver}.tar.gz" "autoconf-${autoconf_ver}.tar.gz"
  cd "$PACKAGES"/autoconf-${autoconf_ver} || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "autoconf"
fi

if build "automake"; then
  download "https://ftp.gnu.org/gnu/automake/automake-${automake_ver}.tar.gz" "automake-${automake_ver}.tar.gz"
  cd "$PACKAGES"/automake-${automake_ver} || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "automake"
fi

if build "libtool"; then
  download "https://ftpmirror.gnu.org/libtool/libtool-${libtool_ver}.tar.gz" "libtool-${libtool_ver}.tar.gz"
  cd "$PACKAGES"/libtool-${libtool_ver} || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --disable-shared
  execute make -j $MJOBS
  execute make install
  build_done "libtool"
fi

if build "cmake"; then
  download "https://cmake.org/files/LatestRelease/cmake-${cmake_ver}.tar.gz" "cmake-${cmake_ver}.tar.gz"
  cd "$PACKAGES"/cmake-${cmake_ver} || exit
  execute env "CC=$(command -v gcc) CXX=$(command -v g++)" ./configure --prefix="${WORKSPACE}" --parallel="${MJOBS}" -- -DCMAKE_USE_OPENSSL=OFF
  execute make -j $MJOBS
  execute make install
  build_done "cmake"
fi


if build "openssl"; then
	download "https://github.com/openssl/openssl/archive/refs/tags/OpenSSL_${openssl_ver}.tar.gz" "openssl-${openssl_ver}.tar.gz"
	cd "$PACKAGES"/openssl-${openssl_ver} || exit
	execute env "$COMPILER_SET" ./config --prefix="${WORKSPACE}" --openssldir="${WORKSPACE}" --with-zlib-include="${WORKSPACE}"/include/ --with-zlib-lib="${WORKSPACE}"/lib no-shared zlib
	execute make -j $MJOBS
	execute make install_sw

	build_done "openssl"
fi
CONFIGURE_OPTIONS+=("--enable-openssl")

# if build "git"; then
#   download "https://www.kernel.org/pub/software/scm/git/git-${git_ver}.tar.xz" "git-${git_ver}.tar.gz"
#   cd "$PACKAGES"/git-${git_ver} || exit
#   execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-openssl --with-zlib="${WORKSPACE}/lib" --with-lib="${WORKSPACE}/lib"
#   execute make -j $MJOBS
#   execute make install
#   build_done "git"
# fi


## Media Libraries

# Lv2 crap
if command_exists $PYTHON; then

	if build "lv2"; then
		download "https://lv2plug.in/spec/lv2-${lv2_ver}.tar.bz2" "lv2-${lv2_ver}.tar.bz2"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --lv2-user
		execute $PYTHON ./waf
		execute $PYTHON ./waf install

		build_done "lv2"
	fi

	if build "waflib"; then
		download "https://gitlab.com/drobilla/autowaf/-/archive/${waflib_ver}/autowaf-${waflib_ver}.tar.gz" "autowaf.tar.gz"
		build_done "waflib"
	fi

	if build "serd"; then
		download "https://gitlab.com/drobilla/serd/-/archive/v${serd_ver}/serd-v${serd_ver}.tar.gz" "serd-v${serd_ver}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/serd-v${serd_ver}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared --no-posix
		execute $PYTHON ./waf
		execute $PYTHON ./waf install
		build_done "serd"
	fi

	if build "pcre"; then
		download "https://ftp.pcre.org/pub/pcre/pcre-${pcre_ver}.tar.gz" "pcre-${pcre_ver}.tar.gz"
		execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
		execute make -j $MJOBS
		execute make install

		build_done "pcre"
	fi

	if build "sord"; then
		download "https://gitlab.com/drobilla/sord/-/archive/v${sord_ver}/sord-v${sord_ver}.tar.gz" "sord-v${sord_ver}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/sord-v${sord_ver}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" CFLAGS="\"${CFLAGS}\"" --static --no-shared --no-utils
		execute $PYTHON ./waf CFLAGS="\"${CFLAGS}\""
		execute $PYTHON ./waf install

		build_done "sord"
	fi

	if build "sratom"; then
		download "https://gitlab.com/lv2/sratom/-/archive/v${sratom_ver}/sratom-v${sratom_ver}.tar.gz" "sratom-v${sratom_ver}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/sratom-v${sratom_ver}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared
		execute $PYTHON ./waf
		execute $PYTHON ./waf install

		build_done "sratom"
	fi

	if build "lilv"; then
		download "https://gitlab.com/lv2/lilv/-/archive/v${lilv_ver}/lilv-v${lilv_ver}.tar.gz" "lilv-v${lilv_ver}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/lilv-v${lilv_ver}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared --no-utils
		execute $PYTHON ./waf
		execute $PYTHON ./waf install
		CFLAGS+=" -I$WORKSPACE/include/lilv-0"
		build_done "lilv"
	fi

	CONFIGURE_OPTIONS+=("--enable-lv2")
fi

## Audio Library
if build "lame"; then
	download "https://netcologne.dl.sourceforge.net/project/lame/lame/${lame_ver}/lame-${lame_ver}.tar.gz" "lame-${lame_ver}.tar.gz"
	cd "$PACKAGES"/lame-${lame_ver} || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "lame"
fi
CONFIGURE_OPTIONS+=("--enable-libmp3lame")

if build "opencore"; then
	download "https://deac-riga.dl.sourceforge.net/project/opencore-amr/opencore-amr/opencore-amr-${opencore_ver}.tar.gz" "opencore-amr-${opencore_ver}.tar.gz"
	cd "$PACKAGES"/opencore-amr-${opencore_ver} || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "opencore"
fi
CONFIGURE_OPTIONS+=("--enable-libopencore_amrnb" "--enable-libopencore_amrwb")

if build "opus"; then
	download "https://archive.mozilla.org/pub/opus/opus-${opus_ver}.tar.gz" "opus-${opus_ver}.tar.gz"
	cd "$PACKAGES"/opus-${opus_ver} || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "opus"
fi
CONFIGURE_OPTIONS+=("--enable-libopus")

if build "libogg"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/ogg/libogg-${libogg_ver}.tar.gz" "libogg-${libogg_ver}.tar.gz"
	cd "$PACKAGES"/libogg-${libogg_ver} || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	build_done "libogg"
fi

if build "libvorbis"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/vorbis/libvorbis-${libvorbis_ver}.tar.gz" "libvorbis-${libvorbis_ver}.tar.gz"
	cd "$PACKAGES"/libvorbis-${libvorbis_ver} || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest
	execute make -j $MJOBS
	execute make install

	build_done "libvorbis"
fi
CONFIGURE_OPTIONS+=("--enable-libvorbis")

if build "libtheora"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/theora/libtheora-${libtheora_ver}.tar.gz" "libtheora-${libtheora_ver}.tar.bz"
	cd "$PACKAGES"/libtheora-${libtheora_ver} || exit
	sed "s/-fforce-addr//g" configure >configure.patched
	chmod +x configure.patched
	mv configure.patched configure
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --with-vorbis-libraries="${WORKSPACE}"/lib --with-vorbis-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest --disable-vorbistest --disable-examples --disable-asm --disable-spec
	execute make -j $MJOBS
	execute make install

	build_done "libtheora"
fi
CONFIGURE_OPTIONS+=("--enable-libtheora")

if build "fdk_aac"; then
	download "https://sourceforge.net/projects/opencore-amr/files/fdk-aac/fdk-aac-${fdk_aac_ver}.tar.gz/download?use_mirror=gigenet" "fdk-aac-${fdk_aac_ver}.tar.gz"
	cd "$PACKAGES"/fdk-aac-${fdk_aac_ver} || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "fdk_aac"
fi
CONFIGURE_OPTIONS+=("--enable-libfdk-aac")

## Image Library
if build "libwebp"; then
	download "https://github.com/webmproject/libwebp/archive/v${libwebp_ver}.tar.gz" "libwebp-${libwebp_ver}.tar.gz"
	make_dir "$PACKAGES"/libwebp-${libwebp_ver}/build
	cd "$PACKAGES"/libwebp-${libwebp_ver}/build || exit
	execute env "$COMPILER_SET" cmake -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_INSTALL_BINDIR=bin -DCMAKE_INSTALL_INCLUDEDIR=include -DENABLE_SHARED=OFF -DENABLE_STATIC=ON ../
	execute make -j $MJOBS
	execute make install

	build_done "libwebp"
fi
CONFIGURE_OPTIONS+=("--enable-libwebp")

if build "libvpx"; then
	download "https://github.com/webmproject/libvpx/archive/v${libvpx_ver}.tar.gz" "libvpx-${libvpx_ver}.tar.gz"
	cd "$PACKAGES"/libvpx-${libvpx_ver} || exit

	if [[ "$OSTYPE" == "darwin"* ]]; then
		echo "Applying Darwin patch"
		sed "s/,--version-script//g" build/make/Makefile >build/make/Makefile.patched
		sed "s/-Wl,--no-undefined -Wl,-soname/-Wl,-undefined,error -Wl,-install_name/g" build/make/Makefile.patched >build/make/Makefile
	fi

	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-unit-tests --disable-shared
	execute make -j $MJOBS
	execute make install

	build_done "libvpx"
fi
CONFIGURE_OPTIONS+=("--enable-libvpx")

## Video Library
if build "xvidcore"; then
	download "https://downloads.xvid.com/downloads/xvidcore-${xvidcore_ver}.tar.gz" "xvidcore-${xvidcore_ver}.tar.gz"
	cd "$PACKAGES"/xvidcore-${xvidcore_ver} || exit
	cd build/generic || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	if [[ -f ${WORKSPACE}/lib/libxvidcore.4.dylib ]]; then
		execute rm "${WORKSPACE}/lib/libxvidcore.4.dylib"
	fi

	if [[ -f ${WORKSPACE}/lib/libxvidcore.so ]]; then
		execute rm "${WORKSPACE}"/lib/libxvidcore.so*
	fi

	build_done "xvidcore"
fi
CONFIGURE_OPTIONS+=("--enable-libxvid")

if build "x264"; then
	download "https://code.videolan.org/videolan/x264/-/archive/${x264_ver}/x264-${x264_ver}.tar.gz" "x264-${x264_ver}.tar.gz"
	cd "$PACKAGES"/x264-${x264_ver} || exit

	if [[ "$OSTYPE" == "linux-gnu" ]]; then
		execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic CXXFLAGS="-fPIC"
	else
		execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic
	fi

	execute make -j $MJOBS
	execute make install
	execute make install-lib-static

	build_done "x264"
fi
CONFIGURE_OPTIONS+=("--enable-libx264")

if build "x265"; then
	download "https://github.com/videolan/x265/archive/Release_${x265_ver}.tar.gz" "x265-${x265_ver}.tar.gz"
	cd "$PACKAGES"/x265-*/ || exit
	cd source || exit
	execute env "$COMPILER_SET" cmake . -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DENABLE_SHARED=off -DBUILD_SHARED_LIBS=OFF
	execute make -j $MJOBS
	execute make install

	if [ -n "$LDEXEFLAGS" ]; then
		sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}/lib/pkgconfig/x265.pc" # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
	fi

	build_done "x265"
fi
CONFIGURE_OPTIONS+=("--enable-libx265")

if build "vid_stab"; then
	download "https://github.com/georgmartius/vid.stab/archive/v${vid_stab_ver}.tar.gz" "vid.stab-${vid_stab_ver}.tar.gz"
	cd "$PACKAGES"/vid.stab-${vid_stab_ver} || exit
	execute env "$COMPILER_SET" cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DUSE_OMP=OFF -DENABLE_SHARED:bool=off .
	execute make
	execute make install

	build_done "vid_stab"
fi
CONFIGURE_OPTIONS+=("--enable-libvidstab")

if build "av1"; then
	#download "https://aomedia.googlesource.com/aom/+archive/0f5cd05bb3d6209e2583ce682d1acd8e21ae24b8.tar.gz" "av1.tar.gz" "av1"
	git clone https://aomedia.googlesource.com/aom "$PACKAGES"/av1
	cd "$PACKAGES"/av1 || exit
	mkdir -p "$PACKAGES"/aom_build
	cd "$PACKAGES"/aom_build || exit
	execute env "$COMPILER_SET" cmake -DENABLE_TESTS=0 -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib "$PACKAGES"/av1
	execute make -j $MJOBS
	execute make install

	build_done "av1"
fi
CONFIGURE_OPTIONS+=("--enable-libaom")

## Other Library
if build "libsdl"; then
	download "https://www.libsdl.org/release/SDL2-${libsdl_ver}.tar.gz"
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "libsdl"
fi

if build "srt"; then
	download "https://github.com/Haivision/srt/archive/v${srt_ver}.tar.gz" "srt-${srt_ver}.tar.gz"
	cd "$PACKAGES"/srt-${srt_ver} || exit
	export OPENSSL_ROOT_DIR="${WORKSPACE}"
	export OPENSSL_LIB_DIR="${WORKSPACE}"/lib
	export OPENSSL_INCLUDE_DIR="${WORKSPACE}"/include/
	execute cmake . -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_INSTALL_BINDIR=bin -DCMAKE_INSTALL_INCLUDEDIR=include -DENABLE_SHARED=OFF -DENABLE_STATIC=ON -DENABLE_APPS=OFF -DUSE_STATIC_LIBSTDCXX=ON
	execute make -j $MJOBS install

	if [ -n "$LDEXEFLAGS" ]; then
		sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}"/lib/pkgconfig/srt.pc # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
	fi

	build_done "srt"
fi
CONFIGURE_OPTIONS+=("--enable-libsrt")

##
## NVCC Stuffs
##
if command_exists "nvcc"; then

	if build "nv-codec"; then
		download "https://github.com/FFmpeg/nv-codec-headers/releases/download/n${nvcodec_ver}/nv-codec-headers-${nvcodec_ver}.tar.gz" "nv-codec-headers-${nvcodec_ver}.tar.gz"
		cd "$PACKAGES"/nv-codec-headers-${nvcodec_ver} || exit
		sed -i "s#PREFIX = /usr/local#PREFIX = ${WORKSPACE}#g" "$PACKAGES"/nv-codec-headers-${nvcodec_ver}/Makefile && execute make install
		build_done "nv-codec"
	fi
	CFLAGS="$CFLAGS -I/usr/lib/cuda/include"
	LDFLAGS="$LDFLAGS -L/usr/lib/cuda/lib64"
	# CONFIGURE_OPTIONS+=("--nvccflags=-gencode arch=compute_52,code=sm_52")
	CONFIGURE_OPTIONS+=("--enable-cuda-nvcc" "--enable-cuvid" "--enable-nvenc" "--enable-cuda-llvm")

	if [ -z "$LDEXEFLAGS" ]; then
		CONFIGURE_OPTIONS+=("--enable-libnpp") # Only libnpp cannot be statically linked.
	fi

	CONFIGURE_OPTIONS+=("--nvccflags=-gencode arch=compute_52,code=sm_52")

	if [ -z "$LDEXEFLAGS" ]; then
		if library_exists "libva"; then
			if build "vaapi"; then
				build_done "vaapi"
			fi
			CONFIGURE_OPTIONS+=("--enable-vaapi")
		fi
	fi

fi

if build "ffmpeg"; then
	#download "https://ffmpeg.org/releases/ffmpeg-snapshot.tar.bz2" "ffmpeg-snapshot.tar.bz2"
	#cd "$PACKAGES"/ffmpeg/ || exit
	[ ! -d "$PACKAGES/FFMpeg" ] && git clone https://github.com/FFmpeg/FFmpeg.git "$PACKAGES"/FFMpeg
	cd "$PACKAGES/FFMpeg" || exit

	env "CC=gcc CXX=g++" ./configure "${CONFIGURE_OPTIONS[@]}" \
		--prefix="${WORKSPACE}" \
		--disable-debug \
		--disable-doc \
		--disable-shared \
		--enable-gpl \
		--enable-nonfree \
		--enable-pthreads \
		--enable-static \
		--enable-small \
		--enable-version3 \
        --extra-libs="${EXTRALIBS}" \
		--pkgconfigdir="$WORKSPACE/lib/pkgconfig" \
		--pkg-config-flags="--static" || exit 1
	execute make -j $MJOBS || exit 1
	execute make install || exit 1

	##
	## Installing the binary!
	##
	echo ""
	echo "Building done. The binary can be found here: $WORKSPACE/bin/ffmpeg"
	echo ""
	verify_binary_type

	if [[ $AUTOINSTALL == "yes" ]]; then
		if [ ! -w "$INSTALL_FOLDER" ]; then
			sudo cp -vfr "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
			sudo cp -vfr "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
			sudo cp -vfr "$WORKSPACE/bin/ffplay" "$INSTALL_FOLDER/ffplay"
			echo "Done. ffmpeg is now installed to your system"
		else
			cp -vfr "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
			cp -vfr "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
			cp -vfr "$WORKSPACE/bin/ffplay" "$INSTALL_FOLDER/ffplay"
			echo "Done. ffmpeg is now installed to your system"
		fi
	elif [[ ! $SKIPINSTALL == "yes" ]]; then
		read -r -p "Install the binary to your $INSTALL_FOLDER folder? [Y/n] " response
		case $response in
		[yY][eE][sS] | [yY])
			if [ ! -w "$INSTALL_FOLDER" ]; then
				sudo cp -vfr "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
				sudo cp -vfr "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
				sudo cp -vfr "$WORKSPACE/bin/ffplay" "$INSTALL_FOLDER/ffplay"
			else
				cp -vfr "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
				cp -vfr "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
				cp -vfr "$WORKSPACE/bin/ffplay" "$INSTALL_FOLDER/ffplay"
			fi
			echo "Done. ffmpeg is now installed to your system"
			;;
		esac
	fi

fi

exit 0
