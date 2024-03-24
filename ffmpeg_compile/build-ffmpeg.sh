#!/bin/bash

# Referenced:
# https://github.com/markus-perl/ffmpeg-build-script
#

VERSION="1.26.2"
CWD="$(pwd -P)"
PACKAGES="$CWD/packages"
WORKSPACE="$CWD/workspace"
CC="$(command -v clang)"
CXX="$(command -v clang++)"
RETRY_DELAY=5

# Get system gcc version
SYSTEM_GCC="/usr/bin/gcc"
SYSTEM_CXX="/usr/bin/g++"
SYSTEM_GCC_ARCH=$("$SYSTEM_GCC" -dumpmachine)
SYSTEM_GCC_VER=$("$SYSTEM_GCC" --version | grep ^gcc | sed 's/^.* //g')
SYSTEM_CXX_MVER=$(echo $SYSTEM_GCC_VER | awk '{ split($0,a,/[.]/); print a[1] }')

# Temporary storage for package version control
LATEST=false
CURRENT_PACKAGE_VERSION=0

# Fallback compilers
if [ ! -x "$(command -v clang)" ]; then
	echo "Oops, clang can't be found!! Falling back to system GCC."
	CC="$(command -v gcc)"
	CXX="$(command -v g++)"
fi

# Setting up build environments
LDLIBSTDCPP="-L/usr/lib/gcc/$SYSTEM_GCC_ARCH/$SYSTEM_CXX_MVER -L/usr/lib -L/usr/lib64"
LDFLAGS_Z="-L${WORKSPACE}/lib"
LDFLAGS="${LDFLAGS_Z} -lz $LDLIBSTDCPP"
LDEXEFLAGS=""
EXTRALIBS="-ldl -lpthread -lm -lz"
case "$CC" in
*/gcc | */clang)
	CFLAGS="-I${WORKSPACE}/include -O3 -march=native -pipe -fomit-frame-pointer -fPIC -fPIE"
	# CXXFLAGS="$CFLAGS"
	CXXFLAGS="$CFLAGS -I/usr/include/c++/$SYSTEM_CXX_MVER -I/usr/include/$SYSTEM_GCC_ARCH/c++/$SYSTEM_CXX_MVER $LDLIBSTDCPP"
	;;
"$SYSTEM_GCC")
	CFLAGS="-I${WORKSPACE}/include -O3 -march=native -pipe -fomit-frame-pointer -fno-semantic-interposition -fPIC -fPIE"
	CXXFLAGS="$CFLAGS"
	;;
*)
	echo "We do not support current compilers..."
	echo "CC=$CC"
	echo "CXX=$CXX"
	exit 1
	;;
esac

# Checking if it is ARM based CPU
MACHINE=`uname -p`

BLD_TYPE="x86_64-unknown-linux-gnu"
if [ "$MACHINE" == "aarch64" ]; then
	BLD_TYPE="aarch64-unknown-linux-gnu"
fi

APPLE_SILICON=false
if [[ ("$(uname -m)" == "arm64") && ("$OSTYPE" == "darwin"*) ]]; then
  # If arm64 AND darwin (macOS)
  export ARCH=arm64
  export MACOSX_DEPLOYMENT_TARGET=11.0
  APPLE_SILICON=true
fi


NVCC_VER_THRSH="8.0.13"

CONFIGURE_OPTIONS=()

INSTALL_FOLDER="$HOME/.local/bin"
if [[ "$OSTYPE" == "darwin"* ]]; then
	INSTALL_FOLDER="/usr/local/bin"
fi

# Setting up pkgconfig stuffs
export PATH="${WORKSPACE}/bin:$PATH"
PKG_CONFIG_PATH="${WORKSPACE}/lib/pkgconfig"
PKG_CONFIG_PATH+="/usr/local/lib/$SYSTEM_GCC_ARCH/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/lib64/$SYSTEM_GCC_ARCH/pkgconfig:/usr/local/lib64/pkgconfig"
PKG_CONFIG_PATH+=":/usr/local/share/pkgconfig:/usr/lib/$SYSTEM_GCC_ARCH/pkgconfig:/usr/lib64/$SYSTEM_GCC_ARCH/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/lib64/pkgconfig"
export PKG_CONFIG_PATH

COMPILER_SET+=" PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\""

# Detect compiler... if it really exists!
if [ ! -x "$(command -v $CC)" ]; then
	echo "$CC"
	echo "No compiler found!!"
	exit 1
fi

# Detect python3
PYTHON="$(command -v python3)"
[ ! -x "$PYTHON" ] && PYTHON='' && echo "No Python found! Lv2 won't be available!"

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
		# curl -L --silent -o "$DOWNLOAD_PATH/$DOWNLOAD_FILE" "$1"
		wget "$1" -O "$DOWNLOAD_PATH/$DOWNLOAD_FILE"

		EXITCODE=$?
		if [ "$EXITCODE" -ne 0 ]; then
			echo ""
			echo "Failed to download $1. Exitcode $EXITCODE. Retrying in $RETRY_DELAY seconds"
			sleep $RETRY_DELAY
			echo "Retrying to download $1...as $DOWNLOAD_FILE"
			# curl -L --silent -o "$DOWNLOAD_PATH/$DOWNLOAD_FILE" "$1"
			wget "$1" -O "$DOWNLOAD_PATH/$DOWNLOAD_FILE"
			
		fi
		
		EXITCODE=$?
		if [ "$EXITCODE" -ne 0 ]; then
            echo ""
            echo "The main download link could have died. Can you provide a mirror?"
            read -r -p "[URL]: " line
            echo "Setting up new download path as..."
            ALT_URL="$line"
            echo "$ALT_URL"
            # curl -L --silent -o "$DOWNLOAD_PATH/$DOWNLOAD_FILE" "$ALT_URL"
            wget "$ALT_URL" -O "$DOWNLOAD_PATH/$DOWNLOAD_FILE"
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
	echo "building $1 - Version $2"
	echo "======================="

	CURRENT_PACKAGE_VERSION="$2"
	if [ -f "$PACKAGES/$1.done" ]; then
		if grep -Fx "$2" "$PACKAGES/$1.done" >/dev/null; then
			echo "$1 version $2 already built. Remove $PACKAGES/$1.done lockfile to rebuild it."
		elif $LATEST; then
			echo "$1 is outdated and will be rebuilt with latest version $2"
		else
			echo "$1 is outdated, but will not be rebuilt. Pass in --latest to rebuild it or remove $PACKAGES/$1.done lockfile."
			return 1
		fi
	fi

	return 0
}

command_exists() {
	if [ -z "$1" ]; then
		return 1
	fi
	
	if ! [[ -x $(command -v "$1") ]]; then
		return 1
	fi

	return 0
}

library_exists() {
	local result=0
	local output=$(pkg-config --exists --print-errors "$1" 2>&1 >/dev/null) || result=$?
	if [ ! "$result" = "0" ]; then
		return 0
	fi
	return 1
}

build_done() {
	touch "$PACKAGES"/"$1.done"
	if [ ! -z "$2" ]; then
		echo "$2" > "$1.done"
	fi
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

# Implemented due to gcc version mismatching problem on Ubuntu...
#
nvcc_ver_chk_ubuntu() {
	vercomp $(get_nvcc_ver) "11.5.0"
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
    echo "     --nosrt: ignore srt build"
    echo "     --sgcc: forces to use /usr/bin/gcc and /usr/bin/g++"
    echo "   --cleanup: remove all working dirs"
	echo "   --clean: Same as cleanlup"
    echo "   --version: shows version info."
    echo "   --help: show this help"
    echo ""
}

bflag=''
cflag=''
nosrt=''
sgcc=''
custom_gcc_suffix=''

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

	    if [[ "$1" == "--cleanup" ]] || [[ "$1" == "--clean"  ]]; then
        cflag='-c'
        cleanup
	    fi

			if [[ "$1" == "--sgcc" ]]; then
				sgcc='sgcc'
			  CC="$SYSTEM_GCC"
			  CXX="$SYSTEM_CXX"
			fi

			if [[ "$1" =~ "--gcc_suffix="* ]]; then
				if [ ! -z "$sgcc" ]; then
					custom_gcc_suffix="$(echo "$1" | awk '{split($1,arr,"="); print arr[length(arr)]}')"
					printf 'Selecting custom GCC with suffix: %s\n' "$custom_gcc_suffix"
					CC="$CC$custom_gcc_suffix"
					CXX="$CXX$custom_gcc_suffix"
				fi
			fi

	    if [[ "$1" == "--nosrt" ]]; then
        	nosrt='yes'
	    fi
		
		if [[ "$1" == "--latest" ]]; then
			LATEST=true
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

COMPILER_SET="CC=\"$CC\" CXX=\"$CXX\" CFLAGS=\"$CFLAGS\" CXXFLAGS=\"$CXXFLAGS\" LDFLAGS=\"$LDFLAGS\" "
COMPILER_SET_Z="CC=\"$CC\" CXX=\"$CXX\" CFLAGS=\"$CFLAGS\" CXXFLAGS=\"$CXXFLAGS\" LDFLAGS=\"$LDFLAGS_Z\" "
COMPILER_SET_GCC="CC=/usr/bin/gcc CXX=/usr/bin/g++ CFLAGS=\"$CFLAGS\" CXXFLAGS=\"$CXXFLAGS\" LDFLAGS=\"$LDFLAGS\" "

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

check_command () {
    if ! command_exists "$1"; then
	    echo "$1 not installed on the system\!\!"
	    exit 1
    fi
}

check_command "make"
check_command "g++"
#check_command "curl"
check_command "wget"


# if ! command_exists "cmake"; then
# 	echo "cmake not installed."
# 	exit 1
# fi

if [ -n "$LDEXEFLAGS" ]; then
	echo "Start the build in full static mode."
fi

## Base Libraries to build stuffs
if build "zlib" "1.3.1"; then
	download "https://www.zlib.net/zlib-${CURRENT_PACKAGE_VERSION}.tar.gz" "zlib-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/zlib-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "${COMPILER_SET_Z}" ./configure --static --prefix="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install
	build_done "zlib"
fi
LDFLAGS+=" -L/zlib/lib"

# -Wno-int-conversion added to CFLAG to avoid error with clang.
# reference: https://gitlab.freedesktop.org/pkg-config/pkg-config/-/issues/77
if build "pkg-config" "0.29.2"; then
	download "https://pkgconfig.freedesktop.org/releases/pkg-config-${CURRENT_PACKAGE_VERSION}.tar.gz" "pkg-config-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/pkg-config-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "${COMPILER_SET} \"CFLAGS=$CFLAGS -Wno-int-conversion\"" ./configure --silent --prefix="${WORKSPACE}" --with-pc-path="${WORKSPACE}"/lib/pkgconfig --with-internal-glib --disable-host-tool -disable-shared 
	execute make -j $MJOBS
	execute make install
	build_done "pkg-config"
fi

if build "yasm" "1.3.0"; then
	download "https://www.tortall.net/projects/yasm/releases/yasm-${CURRENT_PACKAGE_VERSION}.tar.gz" "yasm-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/yasm-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install
	build_done "yasm"
fi

if build "nasm" "2.15.05"; then
	download "https://www.nasm.us/pub/nasm/releasebuilds/${CURRENT_PACKAGE_VERSION}/nasm-${CURRENT_PACKAGE_VERSION}.tar.xz" "nasm-${CURRENT_PACKAGE_VERSION}.tar.xz"
	cd "$PACKAGES"/nasm-${CURRENT_PACKAGE_VERSION} || exit
	execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	build_done "nasm"
fi

if build "libunistring" "0.9.10"; then
	download "https://ftp.gnu.org/gnu/libunistring/libunistring-${CURRENT_PACKAGE_VERSION}.tar.gz" "libunistring-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/libunistring-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	build_done "libunistring"
fi

if build "m4" "1.4.19"; then
  download "https://ftp.gnu.org/gnu/m4/m4-${CURRENT_PACKAGE_VERSION}.tar.gz" "m4-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "${PACKAGES}"/m4-${CURRENT_PACKAGE_VERSION} || exit
  execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "m4"
fi

if build "autoconf" "2.71"; then
  download "https://ftp.gnu.org/gnu/autoconf/autoconf-${CURRENT_PACKAGE_VERSION}.tar.gz" "autoconf-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "$PACKAGES/autoconf-${CURRENT_PACKAGE_VERSION}" || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "autoconf"
fi

if build "automake" "1.16.4"; then
  download "https://ftp.gnu.org/gnu/automake/automake-${CURRENT_PACKAGE_VERSION}.tar.gz" "automake-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "$PACKAGES/automake-${CURRENT_PACKAGE_VERSION}" || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "automake"
fi

if build "libtool" "2.4.6"; then
  download "https://ftpmirror.gnu.org/libtool/libtool-${CURRENT_PACKAGE_VERSION}.tar.gz" "libtool-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "$PACKAGES/libtool-${CURRENT_PACKAGE_VERSION}" || exit
  execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --enable-static --disable-shared
  execute make -j $MJOBS
  execute make install
  build_done "libtool"
fi

if [ ! -x "$(command -v cmake)" ]; then
	if build "cmake" "3.21.2"; then
	  download "https://cmake.org/files/LatestRelease/cmake-${CURRENT_PACKAGE_VERSION}.tar.gz" "cmake-${CURRENT_PACKAGE_VERSION}.tar.gz"
	  cd "$PACKAGES/cmake-${CURRENT_PACKAGE_VERSION}" || exit
	  execute ./configure --prefix="${WORKSPACE}" --parallel="${MJOBS}" -- -DCMAKE_USE_OPENSSL=OFF
	  execute make -j $MJOBS
	  execute make install
	  build_done "cmake"
	fi
fi


if build "openssl" "3.0.13"; then
	download "https://www.openssl.org/source/openssl-${CURRENT_PACKAGE_VERSION}.tar.gz" "openssl-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/openssl-${CURRENT_PACKAGE_VERSION}" || exit 
	if "${APPLE_SILICON}"; then
		sed -n 's/\(##### GNU Hurd\)/"darwin64-arm64-cc" => { \n    inherit_from     => [ "darwin-common", asm("aarch64_asm") ],\n    CFLAGS           => add("-Wall"),\n    cflags           => add("-arch arm64 "),\n    lib_cppflags     => add("-DL_ENDIAN"),\n    bn_ops           => "SIXTY_FOUR_BIT_LONG", \n    perlasm_scheme   => "macosx", \n}, \n\1/g' Configurations/10-main.conf
		execute env "${COMPILER_SET}" ./Configure --prefix="${WORKSPACE}" no-shared no-asm darwin64-arm64-cc
	else	
		execute env "${COMPILER_SET}" ./config --prefix="${WORKSPACE}" --openssldir="${WORKSPACE}" --with-zlib-include="${WORKSPACE}"/include/ --with-zlib-lib="${WORKSPACE}"/lib no-shared zlib
	fi
	execute make -j $MJOBS
	execute make install_sw

	build_done "openssl"
fi 
CONFIGURE_OPTIONS+=("--enable-openssl")

if build "trousers" "0.3.15"; then
	download "https://sourceforge.net/projects/trousers/files/trousers/${CURRENT_PACKAGE_VERSION}/trousers-${CURRENT_PACKAGE_VERSION}.tar.gz/download" "trousers-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/trousers-${CURRENT_PACKAGE_VERSION}" || exit
	execute CC=\"$CC\" CXX=\"$CXX\" sh ./bootstrap.sh
	execute CC=\"$CC\" CXX=\"$CXX\" ./configure \
		--prefix="${WORKSPACE}" \
		--enable-static \
		--disable-shared \
		--with-openssl="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install

	build_done "trousers"
fi

# install git if the systen don't have git... probably won't happen in many cases.
GIT="$(command -v git)"
if [ ! -x  "${GIT}" ]; then
	if build "git" "2.33.0"; then
		download "https://www.kernel.org/pub/software/scm/git/git-${CURRENT_PACKAGE_VERSION}.tar.xz" "git-${CURRENT_PACKAGE_VERSION}.tar.gz"
		cd "$PACKAGES"/git-${CURRENT_PACKAGE_VERSION} || exit
		execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --with-openssl --with-zlib="${WORKSPACE}/lib" --with-lib="${WORKSPACE}/lib"
		execute make -j $MJOBS
		execute make install
		build_done "git"
		GIT="${WORKSPACE}/bin/git"
	fi
fi


## Media Libraries

# Lv2 crap
if command_exists $PYTHON; then

	if build "lv2" "1.18.2"; then
		download "https://lv2plug.in/spec/lv2-${CURRENT_PACKAGE_VERSION}.tar.bz2" "lv2-${CURRENT_PACKAGE_VERSION}.tar.bz2"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --lv2-user
		execute $PYTHON ./waf
		execute $PYTHON ./waf install

		build_done "lv2"
	fi

	if build "waflib" "b600c928b221a001faeab7bd92786d0b25714bc8"; then
		download "https://gitlab.com/drobilla/autowaf/-/archive/${CURRENT_PACKAGE_VERSION}/autowaf-${CURRENT_PACKAGE_VERSION}.tar.gz" "autowaf.tar.gz"
		build_done "waflib"
	fi

	if build "serd" "0.30.10"; then
		download "https://gitlab.com/drobilla/serd/-/archive/v${CURRENT_PACKAGE_VERSION}/serd-v${CURRENT_PACKAGE_VERSION}.tar.gz" "serd-v${CURRENT_PACKAGE_VERSION}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/serd-v${CURRENT_PACKAGE_VERSION}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared --no-posix
		execute $PYTHON ./waf
		execute $PYTHON ./waf install
		build_done "serd"
	fi

	if build "pcre" "8.45"; then
		download "https://sourceforge.net/projects/pcre/files/pcre/${CURRENT_PACKAGE_VERSION}/pcre-${CURRENT_PACKAGE_VERSION}.tar.bz2"
		execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
		execute make -j $MJOBS
		execute make install

		build_done "pcre"
	fi

	if build "sord" "0.16.8"; then
		download "https://gitlab.com/drobilla/sord/-/archive/v${CURRENT_PACKAGE_VERSION}/sord-v${CURRENT_PACKAGE_VERSION}.tar.gz" "sord-v${CURRENT_PACKAGE_VERSION}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/sord-v${CURRENT_PACKAGE_VERSION}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" CFLAGS="\"${CFLAGS}\"" --static --no-shared --no-utils
		execute $PYTHON ./waf CFLAGS="\"${CFLAGS}\""
		execute $PYTHON ./waf install

		build_done "sord"
	fi

	if build "sratom" "0.6.8"; then
		download "https://gitlab.com/lv2/sratom/-/archive/v${CURRENT_PACKAGE_VERSION}/sratom-v${CURRENT_PACKAGE_VERSION}.tar.gz" "sratom-v${CURRENT_PACKAGE_VERSION}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/sratom-v${CURRENT_PACKAGE_VERSION}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared
		execute $PYTHON ./waf
		execute $PYTHON ./waf install

		build_done "sratom"
	fi

	if build "lilv" "0.24.12"; then
		download "https://gitlab.com/lv2/lilv/-/archive/v${CURRENT_PACKAGE_VERSION}/lilv-v${CURRENT_PACKAGE_VERSION}.tar.gz" "lilv-v${CURRENT_PACKAGE_VERSION}.tar.gz"
		execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/lilv-v${CURRENT_PACKAGE_VERSION}/waflib/"
		execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared --no-utils
		execute $PYTHON ./waf
		execute $PYTHON ./waf install
		CFLAGS+=" -I$WORKSPACE/include/lilv-0"
		build_done "lilv"
	fi

	CONFIGURE_OPTIONS+=("--enable-lv2")
fi

## Audio Library
if build "lame" "3.100"; then
	download "https://netcologne.dl.sourceforge.net/project/lame/lame/${CURRENT_PACKAGE_VERSION}/lame-${CURRENT_PACKAGE_VERSION}.tar.gz" "lame-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/lame-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "lame"
fi
CONFIGURE_OPTIONS+=("--enable-libmp3lame")

if build "opencore" "0.1.5"; then
	download "https://sourceforge.net/projects/opencore-amr/files/opencore-amr-${CURRENT_PACKAGE_VERSION}.tar.gz/download?use_mirror=gitenet" "opencore-amr-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/opencore-amr-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "$COMPILER_SET_GCC" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "opencore"
fi
CONFIGURE_OPTIONS+=("--enable-libopencore_amrnb" "--enable-libopencore_amrwb")

if build "opus" "1.3.1"; then
	download "https://archive.mozilla.org/pub/opus/opus-${CURRENT_PACKAGE_VERSION}.tar.gz" "opus-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/opus-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "opus"
fi
CONFIGURE_OPTIONS+=("--enable-libopus")

if build "libogg" "1.3.3"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/ogg/libogg-${CURRENT_PACKAGE_VERSION}.tar.gz" "libogg-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/libogg-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	build_done "libogg"
fi

if build "libvorbis" "1.3.6"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/vorbis/libvorbis-${CURRENT_PACKAGE_VERSION}.tar.gz" "libvorbis-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/libvorbis-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest
	execute make -j $MJOBS
	execute make install

	build_done "libvorbis"
fi
CONFIGURE_OPTIONS+=("--enable-libvorbis")

if build "libtheora" "1.1.1"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/theora/libtheora-${CURRENT_PACKAGE_VERSION}.tar.gz" "libtheora-${CURRENT_PACKAGE_VERSION}.tar.bz"
	cd "$PACKAGES/libtheora-${CURRENT_PACKAGE_VERSION}" || exit
	sed "s/-fforce-addr//g" configure >configure.patched
	chmod +x configure.patched
	mv configure.patched configure
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --build="${BLD_TYPE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --with-vorbis-libraries="${WORKSPACE}"/lib --with-vorbis-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest --disable-vorbistest --disable-examples --disable-asm --disable-spec
	execute make -j $MJOBS
	execute make install

	build_done "libtheora"
fi
CONFIGURE_OPTIONS+=("--enable-libtheora")

if build "fdk_aac" "2.0.2"; then
	download "https://sourceforge.net/projects/opencore-amr/files/fdk-aac/fdk-aac-${CURRENT_PACKAGE_VERSION}.tar.gz/download?use_mirror=gigenet" "fdk-aac-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/fdk-aac-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "fdk_aac"
fi
CONFIGURE_OPTIONS+=("--enable-libfdk-aac")

## Image Library
if build "libwebp" "1.2.1"; then
	download "https://github.com/webmproject/libwebp/archive/v${CURRENT_PACKAGE_VERSION}.tar.gz" "libwebp-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/libwebp-${CURRENT_PACKAGE_VERSION}" && ./autogen.sh
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install

	build_done "libwebp"
fi
CONFIGURE_OPTIONS+=("--enable-libwebp")

if build "libvpx" "1.10.0"; then
	download "https://github.com/webmproject/libvpx/archive/v${CURRENT_PACKAGE_VERSION}.tar.gz" "libvpx-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/libvpx-${CURRENT_PACKAGE_VERSION}" || exit

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
if build "xvidcore" "1.3.7"; then
	# download "https://downloads.xvid.com/downloads/xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz" "xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz"
	download "https://fossies.org/linux/misc/xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz" "xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES/xvidcore-${CURRENT_PACKAGE_VERSION}" || exit
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

if build "x264" "5db6aa6cab1b146e07b60cc1736a01f21da01154"; then
	download "https://code.videolan.org/videolan/x264/-/archive/${CURRENT_PACKAGE_VERSION}/x264-${CURRENT_PACKAGE_VERSION}.tar.gz" "x264-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES"/x264-${CURRENT_PACKAGE_VERSION} || exit

	if [[ "$OSTYPE" == "linux-gnu" ]]; then
		execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic CXXFLAGS=\"$CXXFLAGS\"
	else
		execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic
	fi

	execute make -j $MJOBS
	execute make install
	execute make install-lib-static

	build_done "x264"
fi
CONFIGURE_OPTIONS+=("--enable-libx264")

if build "x265" "3.5"; then
	download "https://github.com/videolan/x265/archive/Release_${CURRENT_PACKAGE_VERSION}.tar.gz" "x265-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "$PACKAGES"/x265-*/ || exit
	cd source || exit
	execute cmake . \
	  -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" \
	  -DCMAKE_C_COMPILER=\""${CC}"\" \
	  -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
	  -DCMAKE_C_FLAGS=\""${CFLAGS}"\" \
	  -DCMAKE_CXX_FLAGS=\""${CXXFLAGS}"\" \
	  -DENABLE_SHARED=OFF \
	  -DBUILD_SHARED_LIBS=OFF 
	execute make -j $MJOBS
	execute make install

	if [ -n "${LDEXEFLAGS}" ]; then
		sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}/lib/pkgconfig/x265.pc" # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
	fi

	build_done "x265"
fi
CONFIGURE_OPTIONS+=("--enable-libx265")

if build "vid_stab" "1.1.0"; then
	download "https://github.com/georgmartius/vid.stab/archive/v${CURRENT_PACKAGE_VERSION}.tar.gz" "vid.stab-${CURRENT_PACKAGE_VERSION}.tar.gz"
	cd "${PACKAGES}/vid.stab-${CURRENT_PACKAGE_VERSION}" || exit
	execute env "${COMPILER_SET}" cmake . \
		-DCMAKE_C_COMPILER=\""${CC}"\" \
		-DCMAKE_CXX_COMPILER=\""${CXX}"\" \
		-DCMAKE_C_FLAGS=\""${CFLAGS}"\" \
		-DCMAKE_CXX_FLAGS=\""${CXXFLAGS}"\" \
		-DBUILD_SHARED_LIBS=OFF \
		-DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" \
		-DUSE_OMP=OFF \
		-DENABLE_SHARED:bool=off 
	execute make
	execute make install

	build_done "vid_stab"
fi
CONFIGURE_OPTIONS+=("--enable-libvidstab")

if build "av1" "Git"; then
	#download "https://aomedia.googlesource.com/aom/+archive/0f5cd05bb3d6209e2583ce682d1acd8e21ae24b8.tar.gz" "av1.tar.gz" "av1"
	git clone https://aomedia.googlesource.com/aom "${PACKAGES}"/av1
	cd "${PACKAGES}"/av1 || exit
	mkdir -p "${PACKAGES}"/aom_build
	cd "${PACKAGES}"/aom_build || exit
	execute env "${COMPILER_SET}" cmake -DENABLE_TESTS=0 -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib "${PACKAGES}"/av1
	execute make -j $MJOBS
	execute make install

	build_done "av1"
fi
CONFIGURE_OPTIONS+=("--enable-libaom")

## Other Library
# if build "libsdl"; then
# 	download "https://www.libsdl.org/release/SDL2-${libsdl_ver}.tar.gz"
# #	execute env "$COMPILER_SET" ./autogen.sh && ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
#     mkdir -p "${PACKAGES}/SDL2-${libsdl_ver}-build" && cd "${PACKAGES}/SDL2-${libsdl_ver}-build" || exit
#     execute cmake -S "${PACKAGES}/SDL2-${libsdl_ver}" -B . \
#     	-DCMAKE_C_COMPILER=\"$CC\" \
#     	-DCMAKE_CXX_COMPILER=\"$CXX\" \
#     	-DCMAKE_C_FLAGS=\"$CFLAGS\" \
#     	-DCMAKE_CXX_FLAGS=\"$CXXFLAGS\" \
#         -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
#         -DCMAKE_INSTALL_LIBDIR="lib" \
#         -DENABLE_SHARED=OFF \
#         -DSDL_STATIC=ON
# 	execute make -j $MJOBS
# 	execute make install

# 	build_done "libsdl"
# fi

if build "libsdl" "2.30.1"; then
  download "https://github.com/libsdl-org/SDL/archive/refs/tags/release-${CURRENT_PACKAGE_VERSION}.tar.gz"
  execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install

  build_done "libsdl" 
fi


if [ ! -n "$nosrt" ]; then
    if build "srt" "1.4.3"; then
	    download "https://github.com/Haivision/srt/archive/v${CURRENT_PACKAGE_VERSION}.tar.gz" "srt-${CURRENT_PACKAGE_VERSION}.tar.gz"
	    cd "$PACKAGES/srt-${CURRENT_PACKAGE_VERSION}" || exit
	    export OPENSSL_ROOT_DIR="${WORKSPACE}"
	    export OPENSSL_LIB_DIR="${WORKSPACE}"/lib
	    export OPENSSL_INCLUDE_DIR="${WORKSPACE}"/include/
	    execute cmake . \
	    	-DCMAKE_C_COMPILER=\""$CC"\" \
	    	-DCMAKE_CXX_COMPILER=\""$CXX"\" \
	    	-DCMAKE_C_FLAGS=\""$CFLAGS"\" \
	    	-DCMAKE_CXX_FLAGS=\""${CXXFLAGS}"\" \
	        -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
	        -DCMAKE_INSTALL_LIBDIR=lib \
	        -DCMAKE_INSTALL_BINDIR=bin \
	        -DCMAKE_INSTALL_INCLUDEDIR=include \
	        -DENABLE_SHARED=OFF \
	        -DENABLE_STATIC=ON \
	        -DENABLE_APPS=OFF \
	        -DUSE_STATIC_LIBSTDCXX=ON
	    execute make -j $MJOBS install

	    if [ -n "$LDEXEFLAGS" ]; then
		    sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}"/lib/pkgconfig/srt.pc # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
	    fi

	    build_done "srt"
    fi
    CONFIGURE_OPTIONS+=("--enable-libsrt")
fi

##
## NVCC Stuffs
##
if command_exists "nvcc"; then

	if build "nv-codec" "12.1.14.0"; then
		download "https://github.com/FFmpeg/nv-codec-headers/releases/download/n${CURRENT_PACKAGE_VERSION}/nv-codec-headers-${CURRENT_PACKAGE_VERSION}.tar.gz" "nv-codec-headers-${CURRENT_PACKAGE_VERSION}.tar.gz"
		cd "$PACKAGES/nv-codec-headers-${CURRENT_PACKAGE_VERSION}" || exit
		sed -i "s#PREFIX = /usr/local#PREFIX = ${WORKSPACE}#g" "$PACKAGES"/nv-codec-headers-${CURRENT_PACKAGE_VERSION}/Makefile && execute make install
		build_done "nv-codec"
	fi

	# Extracting cuda path
	CUDA_BASE_PATH=''
	if [ ! -z "$CUDA_PATH"  ]; then
		CUDA_BASE_PATH="$CUDA_PATH"
	else
		CUDA_BASE_PATH="$( command -v nvcc | awk '{split($1,A,"/"); for (i=1; i<length(A)-1; i++ ) (i==1) ? jp = A[i] : jp = jp"/"A[i]; printf "%s", jp}')"
	fi

	CFLAGS="$CFLAGS -I$CUDA_BASE_PATH/include"
	LDFLAGS="$LDFLAGS -L$CUDA_BASE_PATH/lib64"

	#CONFIGURE_OPTIONS+=("--nvccflags=-gencode arch=compute_52,code=sm_52")
	CONFIGURE_OPTIONS+=("--enable-cuda-nvcc" "--enable-cuvid" "--enable-nvdec" "--enable-nvenc" "--enable-cuda-llvm" "--enable-ffnvcodec")

	if [ -z "$LDEXEFLAGS" ]; then
		CONFIGURE_OPTIONS+=("--enable-libnpp") # Only libnpp cannot be statically linked.
	fi

	CONFIGURE_OPTIONS+=("--nvccflags=\"-allow-unsupported-compiler -gencode arch=compute_52,code=sm_52 \"")

	if [ -z "$LDEXEFLAGS" ]; then
		if library_exists "libva"; then
			if build "vaapi"; then
				build_done "vaapi"su
			fi
			CONFIGURE_OPTIONS+=("--enable-vaapi")
		fi
	fi

	if build "amf" "1.4.33.0"; then
		amf_ver_short="${CURRENT_PACKAGE_VERSION::-2}"
		download "https://github.com/GPUOpen-LibrariesAndSDKs/AMF/archive/refs/tags/v.${amf_ver_short}.tar.gz" "AMF-${amf_ver_short}.tar.gz"
		cd "$PACKAGES/AMF-${amf_ver_short}" || exit
		execute rm -rf "${WORKSPACE}/include/AMF" 
		execute mkdir -p "${WORKSPACE}/include/AMF"
		execute cp -r "${PACKAGES}"/AMF-"${amf_ver_short}"/amf/public/include/* "${WORKSPACE}/include/AMF/"
		build_done "amf" "${amf_ver}"
		CONFIGURE_OPTIONS+=("--enable-amf")
	fi

elif command_exists "clinfo"; then
  CONFIGURE_OPTIONS+=("--enable-opencl")
fi

if build "ffmpeg" "snapshot"; then
	download "https://ffmpeg.org/releases/ffmpeg-${CURRENT_PACKAGE_VERSION}.tar.bz2" "ffmpeg-${CURRENT_PACKAGE_VERSION}.tar.bz2"
	cd "$PACKAGES/ffmpeg-${CURRENT_PACKAGE_VERSION}" || exit

#  if [ "&(nvcc_ver_chk_ubuntu)" = "Fail" ] & [ -x "$(command -v gcc-10)" ]; then
#    CC="$(command -v gcc-10)"
#    CXX="$(command -v g++-10)"
#    CONFIGURE_OPTIONS+=("--nvccflags=\"-ccbin $CC\"")
#  fi

	execute ./configure "${CONFIGURE_OPTIONS[@]}" \
		--prefix="${WORKSPACE}" \
		--cc=\""${CC}"\" \
		--cxx=\""${CXX}"\" \
		--dep-cc=\""${CC}"\" \
		--disable-debug \
		--disable-doc \
		--disable-shared \
		--enable-gpl \
		--enable-nonfree \
		--enable-pthreads \
		--enable-static \
		--enable-small \
		--enable-version3 \
		--extra-cflags=\""${CFLAGS}"\" \
		--extra-cxxflags=\""${CXXFLAGS} ${LDLIBSTDCPP}"\" \
		--extra-ldexeflags=\""${LDEXEFLAGS}"\" \
		--extra-ldflags=\""${LDFLAGS}"\" \
    --extra-libs=\""${EXTRALIBS}"\" \
    --optflags="\"-O3 -march=native -fomit-frame-pointer -pipe\"" \
		--pkgconfigdir=\""${WORKSPACE}/lib/pkgconfig"\" \
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
		[yY][eE][sS] | [yY] | "")
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
