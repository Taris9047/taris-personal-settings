#!/bin/bash

# Referenced:
# https://github.com/markus-perl/ffmpeg-build-script
#

VERSION="1.27.1"
CWD="$(pwd -P)"
PACKAGES="$CWD/packages"
WORKSPACE="$CWD/workspace"

# Some MacOS craps
unset EDITOR VISUAL

# Clang selector
source ./scripts/find_newest_clang.sh

# TODO: Package and URL information file.

RETRY_DELAY=5

# Get system gcc version
SYSTEM_GCC="/usr/bin/gcc"
SYSTEM_CXX="/usr/bin/g++"
SYSTEM_GCC_ARCH=$("$SYSTEM_GCC" -dumpmachine)
# SYSTEM_GCC_VER=$("$SYSTEM_GCC" --version | grep ^gcc | sed 's/^.* //g')
# SYSTEM_CXX_MVER=$(echo $SYSTEM_GCC_VER | awk '{ split($0,a,/[.]/); print a[1] }')
SYSTEM_GCC_VER=$("$SYSTEM_GCC" -dumpfullversion -dumpversion 2>/dev/null || "$SYSTEM_GCC" -dumpversion)
SYSTEM_CXX_MVER="${SYSTEM_GCC_VER%%.*}"

# System CMake minimum version 
CMAKE_SYSTEM_MIN_VER="3.16"

# Detect if the host toolchain requires modern standard overrides
HOST_NEEDS_MODERN_STD=false
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Apple Clang 15+ (Xcode 15+) enforces modern C++ rules similar to GCC 13+
    if [[ -n "$SYSTEM_CXX_MVER" && "$SYSTEM_CXX_MVER" -ge 15 ]]; then
        HOST_NEEDS_MODERN_STD=true
    fi
else
    # Linux (x86_64, Raspberry Pi, Jetson Orin Nano): GNU GCC 13+
    if [[ -n "$SYSTEM_CXX_MVER" && "$SYSTEM_CXX_MVER" -ge 13 ]]; then
        HOST_NEEDS_MODERN_STD=true
    fi
fi

# Temporary storage for package version control
LATEST=false
CURRENT_PACKAGE_VERSION=0

# Compiled binaries
BINARIES=("ffmpeg" "ffprobe" "ffplay")

# Fallback compilers
if [ ! -x "$(command -v clang)" ]; then
  echo "Oops, clang can't be found!! Falling back to system GCC."
  CC="$(command -v gcc)"
  CXX="$(command -v g++)"
fi

# Checking if it is ARM based CPU
# Use uname -m instead of uname -p (uname -p returns "unknown" on Debian/RPi)
MACHINE_ARCH="$(uname -m)"
APPLE_SILICON=false

if [[ "$OSTYPE" == "darwin"* ]]; then
    BLD_TYPE="${MACHINE_ARCH}-apple-darwin"
    if [[ "$MACHINE_ARCH" == "arm64" ]]; then
        export ARCH=arm64
        export MACOSX_DEPLOYMENT_TARGET=11.0
        APPLE_SILICON=true
        MARCH="apple-m1" # Or omit -march on macOS and let -arch arm64 handle it
    else
        MARCH="native"
    fi
    # macOS uses libc++, not /usr/lib/gcc libstdc++
    LDLIBSTDCPP=""
    STDLIB_LINK="-lc++"
else
    BLD_TYPE="${MACHINE_ARCH}-unknown-linux-gnu"
    if [[ "$MACHINE_ARCH" == "aarch64" ]]; then
        # Jetson Orin Nano (Cortex-A78AE) and RPi 4/5 support -mcpu=native
        # GCC on ARM64 prefers -mcpu=native over -march=native; fallback to armv8-a
        if "$SYSTEM_GCC" -mcpu=native -E - </dev/null >/dev/null 2>&1; then
            MARCH="native"
        else
            MARCH="armv8-a"
        fi
    elif [[ "$MACHINE_ARCH" == "armv"* ]]; then
        MARCH="armv7-a"
    else
        MARCH="native"
    fi
    LDLIBSTDCPP="-L/usr/lib/gcc/$SYSTEM_GCC_ARCH/$SYSTEM_CXX_MVER -L/usr/lib -L/usr/lib64"
    STDLIB_LINK="-lstdc++"
fi

CFLAGS_COMMON="-I${WORKSPACE}/include -O3 -pipe -fomit-frame-pointer -fno-semantic-interposition -fPIC -fPIE"
if [[ "$OSTYPE" == "darwin"* && "$APPLE_SILICON" == true ]]; then
    CFLAGS_COMMON+=" -arch arm64"
elif [[ "$MACHINE_ARCH" == "aarch64" && "$MARCH" == "native" ]]; then
    CFLAGS_COMMON+=" -mcpu=native"
else
    CFLAGS_COMMON+=" -march=${MARCH}"
fi

LDFLAGS_Z="-L${WORKSPACE}/lib"
LDFLAGS="${LDFLAGS_Z} -lz ${LDLIBSTDCPP}"
LDEXEFLAGS="-fPIC"
EXTRALIBS="-ldl -lpthread -lm -lz -lmpg123"

# Adjusting CFLAGS for compiler...
case "$CC" in
  "$SYSTEM_GCC")
    CFLAGS="${CFLAGS_COMMON}"
    CXXFLAGS="${CFLAGS}"
  ;;
  */clang)
    # CLANG_GCC_FLAGS="--gcc-install-dir=/usr/lib/gcc/${SYSTEM_GCC_ARCH}/${SYSTEM_CXX_MVER}"
    # CFLAGS="${CLANG_GCC_FLAGS} ${CFLAGS_COMMON}"
    # CXXFLAGS="${CLANG_GCC_FLAGS} ${CFLAGS_COMMON}"
    CFLAGS="${CFLAGS_COMMON}"
    CXXFLAGS="${CFLAGS_COMMON}"
    # CXXFLAGS="${CFLAGS} -I/usr/include/c++/$SYSTEM_CXX_MVER -I/usr/include/$SYSTEM_GCC_ARCH/c++/$SYSTEM_CXX_MVER $LDLIBSTDCPP"
  ;;
  *)
    echo "We do not support current compilers..."
    echo "CC=$CC"
    echo "CXX=$CXX"
    exit 1
  ;;
esac

# Clang math problem?
if [[ "$CC" == *"clang"* ]]; then
  CFLAGS="$CFLAGS"
fi
if [[ "$CXX" == *"clang++"* ]]; then
  CXXFLAGS="$CXXFLAGS"
fi

# Preparing CFLAGS/CXXFLAGS for older standard
# --> This is due to modern LTS Linux systems ship with gcc>16
CFLAGS_COMPAT="-std=gnu90 -pedantic -Wall -Wextra ${CFLAGS}"
CXXFLAGS_COMPAT="-std=gnu++98 -pedantic -Wall -Wextra ${CXXFLAGS}"
CFLAGS_COMPAT11="-std=gnu11 -Wall -Wextra -Wshadow ${CFLAGS}"
CXXFLAGS_COMPAT11="-std=gnu++11 -Wall -Wextra -Wshadow ${CXXFLAGS}"
CFLAGS_COMPAT17="-std=gnu17 -Wall -Wextra -Wshadow ${CFLAGS}"
CXXFLAGS_COMPAT17="-std=gnu++17 -Wall -Wextra -Wshadow ${CXXFLAGS}"

NVCC_VER_THRSH="8.0.13"

CONFIGURE_OPTIONS=()

INSTALL_FOLDER="$HOME/.local/bin"
if [[ "$OSTYPE" == "darwin"* ]]; then
    INSTALL_FOLDER="/usr/local/bin"
fi

# Setting up pkgconfig stuffs
export PATH="${WORKSPACE}/bin:$PATH"
# Force tracking of localized workspace sub-folders on Debian/Trixie
WORKSPACE_ARCH_PC="${WORKSPACE}/lib/${SYSTEM_GCC_ARCH}/pkgconfig"
WORKSPACE_SHARE_PC="${WORKSPACE}/share/pkgconfig"
WORKSPACE_BASE_PC="${WORKSPACE}/lib/pkgconfig"
PKG_CONFIG_PATH="${WORKSPACE_BASE_PC}:${WORKSPACE_ARCH_PC}:${WORKSPACE_SHARE_PC}"
PKG_CONFIG_PATH+=":/usr/local/lib/${SYSTEM_GCC_ARCH}/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/lib64/${SYSTEM_GCC_ARCH}/pkgconfig:/usr/local/lib64/pkgconfig"
PKG_CONFIG_PATH+=":/usr/local/share/pkgconfig:/usr/lib/${SYSTEM_GCC_ARCH}/pkgconfig:/usr/lib64/${SYSTEM_GCC_ARCH}/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/lib64/pkgconfig"
export PKG_CONFIG_PATH

COMPILER_SET+=" PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\""

# Compiler Set reset for MacOS
if [[ "${OSTYPE}" == "darwin"* ]]; then
  MJOBS=$(sysctl -n machdep.cpu.thread_count)
  CONFIGURE_OPTIONS=("--enable-videotoolbox")
  MACOS_LIBTOOL="$(which libtool)"
fi

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

verify_archive() {
    local archive="$1"

    # 1. File must exist and be non-empty
    if [[ ! -s "$archive" ]]; then
        return 1
    fi

    # 2. Reject HTML error pages masquerading as tarballs (e.g. SourceForge/GitHub 404s)
    if command_exists "file"; then
        if file "$archive" | grep -qiE "HTML|ASCII text|empty"; then
            return 1
        fi
    fi

    # 3. Test archive structure without extracting
    case "$archive" in
        *.tar.gz|*.tgz|*.tar.bz2|*.tbz2|*.tar.bz|*.tar.xz|*.txz|*.tar)
            tar -tf "$archive" >/dev/null 2>&1
            return $?
            ;;
        *.zip)
            if command_exists "unzip"; then
                unzip -tq "$archive" >/dev/null 2>&1
                return $?
            fi
            ;;
    esac

    return 0
}

fetch_url() {
    local url="$1"
    local dest="$2"
    if command_exists "wget"; then
        wget --tries=3 --timeout=15 -O "$dest" "$url"
    elif command_exists "curl"; then
        curl -fL --retry 3 --connect-timeout 15 -o "$dest" "$url"
    else
        echo "Error: Neither wget nor curl is installed." >&2
        return 1
    fi
}

download() {
    # download url [filename [dirname]]
    local url="$1"
    local DOWNLOAD_PATH="$PACKAGES"
    local DOWNLOAD_FILE="${2:-"${1##*/}"}"
    local TARGETDIR=""

    if [[ "$DOWNLOAD_FILE" =~ "tar." ]]; then
        TARGETDIR="${DOWNLOAD_FILE%.*}"
        TARGETDIR="${3:-"${TARGETDIR%.*}"}"
    else
        TARGETDIR="${3:-"${DOWNLOAD_FILE%.*}"}"
    fi

    local FILE_PATH="$DOWNLOAD_PATH/$DOWNLOAD_FILE"

    # If a cached file exists, verify it first; delete it if corrupted
    if [[ -f "$FILE_PATH" ]]; then
        if verify_archive "$FILE_PATH"; then
            echo "$DOWNLOAD_FILE is already downloaded and verified."
        else
            echo "⚠️  Cached $DOWNLOAD_FILE is corrupted or incomplete. Removing and re-downloading..."
            rm -f "$FILE_PATH"
        fi
    fi

    # Download if missing (or just removed)
    if [[ ! -f "$FILE_PATH" ]]; then
        echo "Downloading $url as $DOWNLOAD_FILE..."
        if ! fetch_url "$url" "$FILE_PATH" || ! verify_archive "$FILE_PATH"; then
            rm -f "$FILE_PATH"
            echo ""
            echo "Failed to download a valid archive from $url. Retrying in $RETRY_DELAY seconds..."
            sleep "$RETRY_DELAY"
            echo "Retrying download of $url as $DOWNLOAD_FILE..."
            fetch_url "$url" "$FILE_PATH"
        fi

        # If still invalid after retry, ask for mirror
        if ! verify_archive "$FILE_PATH"; then
            rm -f "$FILE_PATH"
            echo ""
            echo "The main download link failed or returned an invalid archive. Can you provide a mirror?"
            read -r -p "[URL]: " ALT_URL
            if [[ -n "$ALT_URL" ]]; then
                echo "Downloading from mirror: $ALT_URL"
                fetch_url "$ALT_URL" "$FILE_PATH"
            fi
        fi

        # Final validation check
        if ! verify_archive "$FILE_PATH"; then
            rm -f "$FILE_PATH"
            echo ""
            echo "❌ Error: Failed to download a valid archive for $DOWNLOAD_FILE"
            exit 1
        fi

        echo "... Done and verified."
    fi

    # Clean and recreate extraction directory to avoid stale partial builds
    remove_dir "$DOWNLOAD_PATH/$TARGETDIR"
    make_dir "$DOWNLOAD_PATH/$TARGETDIR"

    if [[ -n "$3" ]]; then
        if ! tar -xf "$FILE_PATH" -C "$DOWNLOAD_PATH/$TARGETDIR" 2>/dev/null; then
            echo "Failed to extract $DOWNLOAD_FILE"
            exit 1
        fi
    else
        if ! tar -xf "$FILE_PATH" -C "$DOWNLOAD_PATH/$TARGETDIR" --strip-components 1 2>/dev/null; then
            echo "Failed to extract $DOWNLOAD_FILE"
            exit 1
        fi
    fi

    echo "Extracted $DOWNLOAD_FILE"

    cd "$DOWNLOAD_PATH/$TARGETDIR" || {
        echo "Error: Failed to enter directory $DOWNLOAD_PATH/$TARGETDIR"
        exit 1
    }
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
            return 1
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
    pkg-config --exists --print-errors "$1" >/dev/null 2>&1
}

build_done() {
    echo "${2:-$CURRENT_PACKAGE_VERSION}" > "$PACKAGES/$1.done"
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

install_ffmpeg() {
    # Detect if we need sudo by checking if the folder is writable
    local cmd="cp -vfr"
    [[ ! -w "$INSTALL_FOLDER" ]] && cmd="sudo cp -vfr"

    echo "Starting installation to $INSTALL_FOLDER..."
       for bin in "${BINARIES[@]}"; do
        local src="${WORKSPACE}/bin/${bin}"
        local dest="${INSTALL_FOLDER}/${bin}"

        # Check if source exists before trying to copy
        if [[ -f "${src}" ]]; then
            $cmd "${src}" "${dest}"
        else
            echo "⚠️  Skipping: $bin not found at ${src}"
        fi
    done

    echo "✅ Done. ffmpeg installation complete."
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
  if command_exists "nvcc"; then
    # Portable awk/sed extraction (no GNU grep -P required)
    nvcc --version | sed -n 's/^.*release \([0-9][0-9]*\.[0-9][0-9]*\).*$/\1/p'
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

# Portable sed helper
sed_inplace() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        sed -i '' "$@"
    else
        sed -i "$@"
    fi
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

#
# Running the parameter detection.
#
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
        LDEXEFLAGS="-static -fPIC"
        
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
COMPILER_SET_COMPAT="CC=\"$CC\" CXX=\"$CXX\" CFLAGS=\"$CFLAGS_COMPAT\" CXXFLAGS=\"$CXXFLAGS_COMPAT\" LDFLAGS=\"$LDFLAGS\" "

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

if ! command_exists "wget" && ! command_exists "curl"; then
    echo "Neither wget nor curl is installed on the system!!"
    exit 1
fi

# Checking versions.conf 
if [ -f "./scripts/versions.conf" ]; then
  source ./scripts/versions.conf
else
  echo "ERROR: versions.conf configuration tracking file cannot be found!!"
  exit 1
fi

if [ -n "$LDEXEFLAGS" ]; then
    echo "Start the build in full static mode."
fi

## Base Libraries to build stuffs
if build "zlib" "${ZLIB_VER}"; then
    download "${ZLIB_URL}" "zlib-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/zlib-${CURRENT_PACKAGE_VERSION}" || exit
    execute env "${COMPILER_SET_Z}" ./configure --static --prefix="${WORKSPACE}"
    execute make -j $MJOBS
    execute make install
    build_done "zlib" "${CURRENT_PACKAGE_VERSION}"
fi
LDFLAGS+=" -L/zlib/lib"

# -Wno-int-conversion added to CFLAG to avoid error with clang.
# reference: https://gitlab.freedesktop.org/pkg-config/pkg-config/-/issues/77
if build "pkg-config" "${PKG_CONFIG_VER}"; then
    download "${PKG_CONFIG_URL}" "pkg-config-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/pkg-config-${CURRENT_PACKAGE_VERSION}" || exit
    execute env "${COMPILER_SET} \"CFLAGS=$CFLAGS -Wno-int-conversion\"" ./configure --silent --prefix="${WORKSPACE}" --with-pc-path="${WORKSPACE}"/lib/pkgconfig --with-internal-glib --disable-host-tool -disable-shared 
    execute make -j $MJOBS
    execute make install
    build_done "pkg-config" "${CURRENT_PACKAGE_VERSION}"
fi

if build "yasm" "${YASM_VER}"; then
    download "${YASM_URL}" "yasm-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/yasm-${CURRENT_PACKAGE_VERSION}" || exit
    execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}"
    execute make -j $MJOBS
    execute make install
    build_done "yasm" "${CURRENT_PACKAGE_VERSION}"
fi

if build "nasm" "${NASM_VER}"; then
    download "${NASM_URL}" "nasm-${CURRENT_PACKAGE_VERSION}.tar.xz"
    cd "$PACKAGES"/nasm-${CURRENT_PACKAGE_VERSION} || exit
    execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
    execute make -j $MJOBS
    execute make install
    build_done "nasm" "${CURRENT_PACKAGE_VERSION}"
fi

if build "libunistring" "${LIBUNISTRING_VER}"; then
    download "${LIBUNISTRING_URL}" "libunistring-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/libunistring-${CURRENT_PACKAGE_VERSION}" || exit
    execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --enable-static
    execute make -j $MJOBS
    execute make install
    build_done "libunistring" "${CURRENT_PACKAGE_VERSION}"
fi

if build "m4" "${M4_VER}"; then
  download "${M4_URL}" "m4-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "${PACKAGES}"/m4-${CURRENT_PACKAGE_VERSION} || exit
  execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "m4" "${CURRENT_PACKAGE_VERSION}"
fi

if build "autoconf" "${AUTOCONF_VER}"; then
  download "${AUTOCONF_URL}" "autoconf-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "$PACKAGES/autoconf-${CURRENT_PACKAGE_VERSION}" || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "autoconf" "${CURRENT_PACKAGE_VERSION}"
fi

if build "automake" "${AUTOMAKE_VER}"; then
  download "${AUTOMAKE_URL}" "automake-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "$PACKAGES/automake-${CURRENT_PACKAGE_VERSION}" || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "automake" "${CURRENT_PACKAGE_VERSION}"
fi

if build "help2man" "${HELP2MAN_VER}"; then
    download "${HELP2MAN_URL}" "help2man-${CURRENT_PACKAGE_VERSION}.tar.xz"
    execute ./configure --prefix="${WORKSPACE}"
    execute make -j "${MJOBS}"
    execute make install
    build_done "help2man" "${CURRENT_PACKAGE_VERSION}"
fi

if build "libtool" "${LIBTOOL_VER}"; then
  download "${LIBTOOL_URL}" "libtool-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "$PACKAGES/libtool-${CURRENT_PACKAGE_VERSION}" || exit
#   if [ -d "${PACKAGES}/libtool*" ]; then
#       rm -rf "${PACKAGES}/libtool*"
#   fi
  #git clone --depth 1 git://git.savannah.gnu.org/libtool.git "${PACKAGES}/libtool" && cd "${PACKAGES}/libtool"
  #execute ./bootstrap
  execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --enable-static --disable-shared
  execute make -j $MJOBS
  execute make install
  build_done "libtool" "${CURRENT_PACKAGE_VERSION}"
fi

# Handling CMake
get_cmake_ver() {
    if command_exists "cmake"; then
        # Extracts X.Y.Z portably across macOS and Linux
        cmake --version | head -n 1 | sed -n 's/^.*version \([0-9][0-9.]*\).*$/\1/p'
    else
        echo "0.0.0"
    fi
}

cmake_ver_chk() {
    # Requires strictly greater than CMAKE_SYSTEM_MIN_VER (return code 1)
    # Note: If CMAKE_SYSTEM_MIN_VER itself is acceptable, allow return code 0 as well.
    vercomp "$(get_cmake_ver)" "${CMAKE_SYSTEM_MIN_VER}"
    case $? in
        1) return 0 ;; # > CMAKE_SYSTEM_MIN_VER (Pass)
        *) return 1 ;; # <= CMAKE_SYSTEM_MIN_VER (Fail)
    esac
}

if cmake_ver_chk; then
    echo "Found modern system CMake ($(get_cmake_ver) > ${CMAKE_SYSTEM_MIN_VER}). Skipping manual compile."
    make_dir "${WORKSPACE}/bin"
    ln -sf "$(command -v cmake)" "${WORKSPACE}/bin/cmake"
elif build "cmake" "${CMAKE_VER}"; then
    echo "Deleteing previousely downloaded cmake packages..."
    rm -rf "${PACKAGES}/cmake-${CURRENT_PACKAGE_VERSION}"
      download "${CMAKE_URL}" "cmake-${CURRENT_PACKAGE_VERSION}.tar.gz"
      cd "${PACKAGES}/cmake-${CURRENT_PACKAGE_VERSION}" || exit
      
      if [[ "$OSTYPE" == "darwin"* ]]; then
          CMAKE_BOOT_LDFLAGS="-L${WORKSPACE}/lib"
      else
          CMAKE_BOOT_LDFLAGS="-B/usr/bin/ -Wl,-rpath,/usr/lib/${SYSTEM_GCC_ARCH} -Wl,-rpath-link,/usr/lib/${SYSTEM_GCC_ARCH}"
      fi
      
      execute \
        CC=\"${SYSTEM_GCC}\" CXX=\"${SYSTEM_CXX}\" \
        CFLAGS=\"${CFLAGS_COMMON}\" CXXFLAGS=\"${CFLAGS_COMMON}\" \
        LDFLAGS=\"${CMAKE_BOOT_LDFLAGS}\" \
        ./bootstrap --prefix=\"${WORKSPACE}\" --parallel="${MJOBS}" \
        --no-system-libs -- -DCMAKE_USE_OPENSSL=OFF
      execute make -j $MJOBS
      execute make install
      build_done "cmake" "${CURRENT_PACKAGE_VERSION}"
fi


if build "openssl" "${OPENSSL_VER}"; then
    download "${OPENSSL_URL}" "openssl-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/openssl-${CURRENT_PACKAGE_VERSION}" || exit 
    if "${APPLE_SILICON}"; then
        sed -n 's/\(##### GNU Hurd\)/"darwin64-arm64-cc" => { \n    inherit_from     => [ "darwin-common", asm("aarch64_asm") ],\n    CFLAGS           => add("-Wall"),\n    cflags           => add("-arch arm64 "),\n    lib_cppflags     => add("-DL_ENDIAN"),\n    bn_ops           => "SIXTY_FOUR_BIT_LONG", \n    perlasm_scheme   => "macosx", \n}, \n\1/g' Configurations/10-main.conf
        execute env "${COMPILER_SET}" ./Configure --prefix="${WORKSPACE}" --libdir=lib no-shared no-asm darwin64-arm64-cc
    else    
      execute env "${COMPILER_SET}" ./config --prefix="${WORKSPACE}" --libdir=lib --openssldir="${WORKSPACE}" no-shared zlib --with-zlib-include="${WORKSPACE}"/include/ --with-zlib-lib="${WORKSPACE}"/lib 
    fi
    execute make -j $MJOBS
    execute make install_sw

    build_done "openssl" "${CURRENT_PACKAGE_VERSION}"
fi 
CONFIGURE_OPTIONS+=("--enable-openssl")

#if build "trousers" "0.3.15"; then
#  ARCH="$(uname -m)"
#  OPENSSL_LIB="/usr"
#  if [[ "x86_64" == "${ARCH}" ]]; then
#    OPENSSL_LIB="${WORKSPACE}"
#  fi
#   download "https://sourceforge.net/projects/trousers/files/trousers/${CURRENT_PACKAGE_VERSION}/trousers-${CURRENT_PACKAGE_VERSION}.tar.gz/download" "trousers-${CURRENT_PACKAGE_VERSION}.tar.gz"
#   cd "$PACKAGES/trousers-${CURRENT_PACKAGE_VERSION}" || exit
#   execute CC=\"$CC\" CXX=\"$CXX\" sh ./bootstrap.sh
#   execute CC=\"$CC\" CXX=\"$CXX\" ./configure \
#       --prefix="${WORKSPACE}" \
#       --enable-static \
#       --with-openssl="${OPENSSL_LIB}"
#   execute make -j $MJOBS
#   execute make install

#   build_done "trousers" "${CURRENT_PACKAGE_VERSION}"
#fi

# install git if the systen don't have git... But really? not having git is extra ordinary situation these days...
GIT="$(command -v git)"
if [ ! -x  "${GIT}" ]; then
    if build "git" "${GIT_VER}"; then
        download "${GIT_URL}" "git-${CURRENT_PACKAGE_VERSION}.tar.gz"
        cd "$PACKAGES"/git-${CURRENT_PACKAGE_VERSION} || exit
#       execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --with-openssl --with-zlib="${WORKSPACE}/lib" --with-lib="${WORKSPACE}/lib"
        execute make -j $MJOBS
        execute make install
        build_done "git" "${CURRENT_PACKAGE_VERSION}"
        GIT="${WORKSPACE}/bin/git"
    fi
fi

# if build "giflib" "6.1.3"; then
#   download "https://sourceforge.net/projects/giflib/files/giflib-6.x/giflib-${CURRENT_PACKAGE_VERSION}.tar.gz"
#   if [[ "$OSTYPE" == "darwin"* ]]; then
#     wget  "https://raw.githubusercontent.com/pld-linux/giflib/refs/heads/master/giflib-make.patch" -O Makefile.patch || true
#     execute patch -p0 --forward "${PACKAGES}/giflib-${CURRENT_PACKAGE_VERSION}/Makefile" "${PACKAGES}/giflib-${CURRENT_PACKAGE_VERSION}/Makefile.patch" || true
#   fi
#   cd "${PACKAGES}"/giflib-$CURRENT_PACKAGE_VERSION || exit
#   #multicore build disabled for this library
#   execute make
#   execute make PREFIX="${WORKSPACE}" install
#   build_done "giflib" $CURRENT_PACKAGE_VERSION
# fi


if build "lcms2" "${LCMS2_VER}"; then
  download "${LCMS2_URL}" "lcms2-${LCMS2_VER}.tar.gz"
  execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j "$MJOBS"
  execute make install
  build_done "lcms2" "$CURRENT_PACKAGE_VERSION"
fi
# lcms2 is a libjxl dependency, but ffmpeg also uses it for ICC profile support
# in the image decoders and for the iccdetect and iccgen filters.
CONFIGURE_OPTIONS+=("--enable-lcms2")


## Media Libraries

# Lv2 crap -- Now the build system is meson not waf. So skipping it for now..
if command_exists "meson"; then
    if command_exists $PYTHON; then

        if build "zix" "${ZIX_VER}"; then
            download "${ZIX_URL}"
            execute env "${COMPILER_SET}" meson setup build -Ddefault_library=static -Db_staticpic=true --prefix="${WORKSPACE}"
            cd ./build
            execute meson compile
            execute meson install

            build_done "zix" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "lv2" "${LV2_VER}"; then
            download "${LV2_URL}" "lv2-${CURRENT_PACKAGE_VERSION}.tar.xz"
            # execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --lv2-user
            # execute $PYTHON ./waf
            # execute $PYTHON ./waf install
            execute env "${COMPILER_SET}" meson setup build -Ddefault_library=static -Db_staticpic=true -Db_pie=true 
            cd ./build
            execute meson configure -Dprefix="${WORKSPACE}" 
            execute meson compile
            execute meson install
            build_done "lv2" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "waflib" "b600c928b221a001faeab7bd92786d0b25714bc8"; then
            download "${WAFLIB_URL}" "autowaf.tar.gz"
            build_done "waflib" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "serd" "${SERD_VER}"; then
            download "${SERD_URL}" "serd-v${CURRENT_PACKAGE_VERSION}.tar.gz"
            execute env "${COMPILER_SET}" meson setup build -Db_staticpic=true -Ddefault_library=static --prefix="${WORKSPACE}"
            cd ./build
            execute meson compile
            execute meson install

            build_done "serd" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "pcre" "${PCRE_VER}"; then
            download "${PCRE_URL}"
            execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
            execute make -j $MJOBS
            execute make install

            build_done "pcre" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "sord" "${SORD_VER}"; then
            download "${SORD_URL}" "sord-${CURRENT_PACKAGE_VERSION}.tar.xz"
            execute env "${COMPILER_SET}" meson setup build -Dc_args="-I${WORKSPACE}/include" -Db_staticpic=true -Ddefault_library=static --prefix="${WORKSPACE}"
            cd ./build
            execute meson compile
            execute meson install

            build_done "sord" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "sratom" "${SRATOM_VER}"; then
            download "${SRATOM_URL}" "sratom-v${CURRENT_PACKAGE_VERSION}.tar.gz"
            execute env "${COMPILER_SET}" meson setup build -Db_staticpic=true -Ddefault_library=static
            cd ./build
            execute meson configure -Dprefix="${WORKSPACE}"
            execute meson compile
            execute meson install           

            build_done "sratom" "${CURRENT_PACKAGE_VERSION}"
        fi

        if build "lilv" "${LILV_VER}"; then
            download "${LILV_URL}" "lilv-${CURRENT_PACKAGE_VERSION}.tar.xz"
            execute env "${COMPILER_SET}" meson setup build -Db_staticpic=true -Ddefault_library=static -Dbindings_cpp=disabled --prefix="${WORKSPACE}"
            cd ./build
            execute meson compile
            execute meson install   

            #CFLAGS+=" -I${WORKSPACE}/include/lilv-0"
            #CXXFLAGS="${CFLAGS}"

            build_done "lilv" "${CURRENT_PACKAGE_VERSION}"
        fi

        CONFIGURE_OPTIONS+=("--enable-lv2")
    fi
fi

## Audio Library
if build "libmpg123" "${LIBMPG123_VER}"; then
  download "${LIBMPG123_URL}" "mpg123-${LIBMPG123_VER}.tar.bz2"
  cd "${PACKAGES}/mpg123-${CURRENT_PACKAGE_VERSION}" || exit
  execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --enable-static --disable-shared
  execute make -j "${MJOBS}"
  execute make install

  build_done "libmpg123" "${CURRENT_PACKAGE_VERSION}"
fi

if build "lame" "4.0"; then
  download "${LAME_URL}"
    cd "$PACKAGES/lame-${CURRENT_PACKAGE_VERSION}" || exit
  LAME_CFLAGS="\"${CFLAGS} -std=gnu89 -Wno-error=implicit-function-declaration -Wno-error=incompatible-pointer-types\""
  if [[ "$OSTYPE" == "darwin"* ]]; then
    awk '/#include/ && !done {print "#include <locale.h>"; done=1} {print}' frontend/parse.c > temp.c && mv temp.c frontend/parse.c
  fi
  sed_inplace -e 's/^\(\s*hardcode_libdir_flag_spec\s*=\).*/\1/' configure
    execute env "CC=${CC} CFLAGS=${LAME_CFLAGS}" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
    execute make -j $MJOBS
    execute make install

    build_done "lame" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libmp3lame")

if build "libogg" "${LIBOGG_VER}"; then
  download "${LIBOGG_URL}"
  cd "$PACKAGES/libogg-${CURRENT_PACKAGE_VERSION}/" || exit
  execute env "${COMPILER_SET}" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static --docdir="${WORKSPACE}/share/doc/libogg-${CURRENT_PACKAGE_VERSION}"
  execute make -j $MJOBS
  execute make install

  build_done "libogg" "${CURRENT_PACKAGE_VERSION}"
fi


if build "libvorbis" "${LIBVORBIS_VER}"; then
    download "${LIBVORBIS_URL}" "libvorbis-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/libvorbis-${CURRENT_PACKAGE_VERSION}" || exit 
  sed "s/-force_cpusubtype_ALL//g" configure.ac >configure.ac.patched && \
    rm configure.ac && \
    mv configure.ac.patched configure.ac
  execute env "$COMPILER_SET" ./autogen.sh --prefix="${WORKSPACE}"
    execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest
    execute make -j $MJOBS
    execute make install

    build_done "libvorbis" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libvorbis")

if build "libtheora" "${LIBTHEORA_VER}"; then
    download "${LIBTHEORA_URL}" "libtheora-${CURRENT_PACKAGE_VERSION}.tar.bz"
    cd "$PACKAGES/libtheora-${CURRENT_PACKAGE_VERSION}" || exit
    sed "s/-fforce-addr//g" configure >configure.patched
    chmod +x configure.patched
    mv configure.patched configure
    
    # Replace 2009 config.sub / config.guess with modern versions from workspace libtool/automake
    for aux_dir in "${WORKSPACE}"/share/libtool/build-aux "${WORKSPACE}"/share/automake-*; do
     if [[ -f "${aux_dir}/config.sub" ]]; then
       cp -f "${aux_dir}/config.sub" "${aux_dir}/config.guess" .
       break
     fi
   done

    execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --build="${BLD_TYPE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --with-vorbis-libraries="${WORKSPACE}"/lib --with-vorbis-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest --disable-vorbistest --disable-examples --disable-asm --disable-spec
    execute make -j $MJOBS
    execute make install

    build_done "libtheora" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libtheora")

if build "fdk_aac" "${FDK_AAC_VER}"; then
    download "${FDK_AAC_URL}" "fdk-aac-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/fdk-aac-${CURRENT_PACKAGE_VERSION}" || exit
    execute env "$COMPILER_SET_COMPAT" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
    execute make -j $MJOBS
    execute make install

    build_done "fdk_aac"
fi
CONFIGURE_OPTIONS+=("--enable-libfdk-aac")

## Image Library
if build "libwebp" "${LIBWEBP_VER}"; then
  download "${LIBWEBP_URL}"
    cd "$PACKAGES/libwebp-${CURRENT_PACKAGE_VERSION}" && ./autogen.sh
    execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static --enable-libwebpmux --enable-libwebpdemux --enable-libwebpdecoder --enable-libwebpextras --enable-swap-16bit-csp
    execute make -j $MJOBS
    execute make install

    build_done "libwebp" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libwebp")

if build "libvpx" "${LIBVPX_VER}"; then
    download "${LIBVPX_URL}" "libvpx-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "$PACKAGES/libvpx-${CURRENT_PACKAGE_VERSION}" || exit

  if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Applying Darwin patch"
    sed "s/,--version-script//g" build/make/Makefile >build/make/Makefile.patched
    sed "s/-Wl,--no-undefined -Wl,-soname/-Wl,-undefined,error -Wl,-install_name/g" build/make/Makefile.patched >build/make/Makefile
  fi
  
  COMPILER_SET_BCK="${COMPILER_SET}"
    execute env ./configure --prefix="${WORKSPACE}" --disable-unit-tests --disable-shared --disable-examples --as=yasm --enable-vp9-highbitdepth
    execute make -j $MJOBS
    execute make install

    build_done "libvpx" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libvpx")

## Video Library
# Xvidcore
if build "xvidcore" "${XVIDCORE_VER}"; then
    # download "https://downloads.xvid.com/downloads/xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz" "xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz"
    download "${XVIDCORE_URL}" "xvidcore-${CURRENT_PACKAGE_VERSION}.tar.gz"
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

    build_done "xvidcore" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libxvid")

# x264
if build "x264" "${X264_VER}"; then
  rm -rf "${PACKAGES}/x264-git"
  git clone --depth 1 "${X264_URL}" "${PACKAGES}/x264-git"
  cd "${PACKAGES}/x264-git" || exit

    if [[ "$OSTYPE" == "linux-gnu" ]]; then
        execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic CXXFLAGS=\"$CXXFLAGS\"
    else
        execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic
    fi

    execute make -j $MJOBS
    execute make install
    execute make install-lib-static

    build_done "x264" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libx264")

# X265
if build "x265" "${X265_VER}"; then
  download "${X265_URL}" "x265-${X265_VER}.tar.gz"
  # download "http://ftp.videolan.org/pub/videolan/x265/x265_${CURRENT_PACKAGE_VERSION}.tar.gz" "x265-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "${PACKAGES}/x265-${CURRENT_PACKAGE_VERSION}" || exit
  
  # Defining CXX flags for x265
  CFLAGS_X265="${CFLAGS_COMPAT11} -Wno-error=invalid-constexpr"
  CXXFLAGS_X265="${CXXFLAGS_COMPAT11} -Wno-error=invalid-constexpr -Wno-register"
  
  if [ "${HOST_NEEDS_MODERN_STD}" = true ]; then
    # 1. Force CMakeLists to use modern C++17 definitions instead of hardcoded C++11/C++98
    sed_inplace 's/add_definitions(-std=gnu++11)/add_definitions(-std=gnu++17 -Wno-invalid-constexpr)/g' source/CMakeLists.txt
    sed_inplace 's/add_definitions(-std=gnu++98)/add_definitions(-std=gnu++17 -Wno-invalid-constexpr)/g' source/CMakeLists.txt

    # 2. Force the matching C standard definition up from C11 to C17
    sed_inplace 's/-std=gnu11/-std=gnu17/g' source/CMakeLists.txt

    # 3. Strip out any hardcoded global -pedantic rules that cause compilation failures
    sed_inplace 's/-pedantic//g' source/CMakeLists.txt

    CXXFLAGS_X265+=" -include cstdint"
  else
    CXXFLAGS_X265+=" -include stdint.h"
  fi
  
  X265_SRC_DIR="${PACKAGES}/x265-${CURRENT_PACKAGE_VERSION}/source"
  
  cd build/linux || exit
  
  if [[ "$OSTYPE" == "darwin"* ]]; then
      # Standard single-pass 8-bit build for macOS to avoid libtool symbol-stripping bugs
      execute "${WORKSPACE}"/bin/cmake "${X265_SRC_DIR}" \
        -DCMAKE_C_COMPILER=\""${CC}"\" \
        -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
        -DCMAKE_C_FLAGS=\""${CFLAGS_X265}"\" \
        -DCMAKE_CXX_FLAGS=\""${CXXFLAGS_X265}"\" \
        -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
        -DENABLE_SHARED=OFF \
        -DBUILD_SHARED_LIBS=OFF \
        -DENABLE_CLI=OFF
      execute make -j $MJOBS
      execute make install
  else
  
    rm -rf 8bit 10bit 12bit 2>/dev/null
    mkdir -p 8bit 10bit 12bit
  
    # Working on 12 bit
    echo "Building 12 bit"
    cd 12bit || exit
    execute "${WORKSPACE}"/bin/cmake "${X265_SRC_DIR}" \
      -DCMAKE_C_COMPILER=\""${CC}"\" \
      -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
      -DCMAKE_C_FLAGS=\""${CFLAGS_X265}"\" \
      -DCMAKE_CXX_FLAGS=\""${CXXFLAGS_X265}"\" \
      -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
      -DENABLE_SHARED=OFF \
      -DBUILD_SHARED_LIBS=OFF \
      -DHIGH_BIT_DEPTH=ON \
      -DENABLE_HDR10_PLUS=ON \
      -DEXPORT_C_API=OFF \
      -DENABLE_CLI=OFF \
      -DMAIN12=ON
    execute make -j $MJOBS

    # 10 bit
    echo "Building 10 bit"
    cd ../10bit || exit
    execute "${WORKSPACE}"/bin/cmake "${X265_SRC_DIR}" \
      -DCMAKE_C_COMPILER=\""${CC}"\" \
      -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
      -DCMAKE_C_FLAGS=\""${CFLAGS_X265}"\" \
      -DCMAKE_CXX_FLAGS=\""${CXXFLAGS_X265}"\" \
      -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
      -DENABLE_SHARED=OFF \
      -DBUILD_SHARED_LIBS=OFF \
      -DHIGH_BIT_DEPTH=ON \
      -DENABLE_HDR10_PLUS=ON \
      -DEXPORT_C_API=OFF \
      -DENABLE_CLI=OFF
    execute  make -j $MJOBS

    # Lastly 8 bit
    echo "Building main: 8 bit"
    cd ../8bit || exit
    ln -sf ../10bit/libx265.a libx265_main10.a
    ln -sf ../12bit/libx265.a libx265_main12.a
    execute "${WORKSPACE}"/bin/cmake "${X265_SRC_DIR}" \
      -DCMAKE_C_COMPILER=\""${CC}"\" \
      -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
      -DCMAKE_C_FLAGS=\""${CFLAGS_X265}"\" \
      -DCMAKE_CXX_FLAGS=\""${CXXFLAGS_X265}"\" \
      -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
      -DENABLE_SHARED=OFF \
      -DBUILD_SHARED_LIBS=OFF \
      -DEXTRA_LIB=\"x265_main10.a\;x265_main12.a\;-ldl\" \
      -DEXTRA_LINK_FLAGS=-L. \
      -DLINKED_10BIT=ON \
      -DLINKED_12BIT=ON
    execute make -j $MJOBS

    mv libx265.a libx265_main.a

    execute ar -M <<EOF
CREATE libx265.a
ADDLIB libx265_main.a
ADDLIB libx265_main10.a
ADDLIB libx265_main12.a
SAVE
END
EOF
 
    execute make install
  fi
 
  build_done "x265" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libx265")


# Vid stab
if build "vid_stab" "${VID_STAB_VER}"; then
    download "${VID_STAB_URL}" "vid.stab-${CURRENT_PACKAGE_VERSION}.tar.gz"
    cd "${PACKAGES}/vid.stab-${CURRENT_PACKAGE_VERSION}" || exit
    execute env "${COMPILER_SET}" cmake . \
        -DCMAKE_C_COMPILER=\""${CC}"\" \
        -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
        -DCMAKE_C_FLAGS=\""${CFLAGS}"\" \
        -DCMAKE_CXX_FLAGS=\""${CXXFLAGS}"\" \
        -DBUILD_SHARED_LIBS=OFF \
        -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" \
        -DUSE_OMP=OFF \
        -DCMAKE_POLICY_VERSION_MINIMUM="${CMAKE_SYSTEM_MIN_VER}" \
        -DENABLE_SHARED:bool=off 
    execute make
    execute make install

    build_done "vid_stab" "${CURRENT_PACKAGE_VERSION}"
fi
CONFIGURE_OPTIONS+=("--enable-libvidstab")

if build "av1" "${AV1_VER}"; then
    #download "https://aomedia.googlesource.com/aom/+archive/0f5cd05bb3d6209e2583ce682d1acd8e21ae24b8.tar.gz" "av1.tar.gz" "av1"
    rm -rf "${PACKAGES}/av1"
    git clone --depth 1 "${AV1_URL}" "${PACKAGES}"/av1
    cd "${PACKAGES}"/av1 || exit
    mkdir -p "${PACKAGES}"/aom_build
    cd "${PACKAGES}"/aom_build || exit
    execute env "${COMPILER_SET}" cmake -DENABLE_TESTS=0 -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib "${PACKAGES}"/av1
    execute make -j $MJOBS
    execute make install

    build_done "av1" "GIT"
fi
CONFIGURE_OPTIONS+=("--enable-libaom")

## Other Library
# if build "libsdl"; then
#   download "https://www.libsdl.org/release/SDL2-${libsdl_ver}.tar.gz"
# # execute env "$COMPILER_SET" ./autogen.sh && ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
#     mkdir -p "${PACKAGES}/SDL2-${libsdl_ver}-build" && cd "${PACKAGES}/SDL2-${libsdl_ver}-build" || exit
#     execute cmake -S "${PACKAGES}/SDL2-${libsdl_ver}" -B . \
#       -DCMAKE_C_COMPILER=\"$CC\" \
#       -DCMAKE_CXX_COMPILER=\"$CXX\" \
#       -DCMAKE_C_FLAGS=\"$CFLAGS\" \
#       -DCMAKE_CXX_FLAGS=\"$CXXFLAGS\" \
#         -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
#         -DCMAKE_INSTALL_LIBDIR="lib" \
#         -DENABLE_SHARED=OFF \
#         -DSDL_STATIC=ON
#   execute make -j $MJOBS
#   execute make install

#   build_done "libsdl"
# fi

if build "libsdl" "${LIBSDL_VER}"; then
  download "${LIBSDL_URL}" "libsdl-${CURRENT_PACKAGE_VERSION}.tar.gz"
  cd "${PACKAGES}/libsdl-${CURRENT_PACKAGE_VERSION}" || exit
  rm -rfv ./build
  mkdir ./build && cd ./build 
  execute cmake -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
        -DCMAKE_BUILD_TYPE="Release" \
        -DSDL_TEST_LIBRARY=OFF \
        -DSDL_STATIC=ON \
        -DSDL_SHARED=OFF \
        -DSDL_RPATH=OFF \
        -DCMAKE_C_COMPILER=\"${CC}\" \
        -DCMAKE_CXX_COMPILER=\"${CXX}\" \
        -DCMAKE_C_FLAGS=\"${CFLAGS}\" \
        -DCMAKE_CXX_FLAGS=\"${CXXFLAGS}\" \
        -DSDL_UNIX_CONSOLE_BUILD=ON \
        -DSDL_X11=OFF \
        -DSDL_WAYLAND=OFF \
        -W no-author -G \"Unix Makefiles\" \
        ..
  execute make -j $MJOBS
  execute make install
  cd ..

  #execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  #execute make -j $MJOBS
  #execute make install

  build_done "libsdl" "${CURRENT_PACKAGE_VERSION}"
fi


if [ ! -n "$nosrt" ]; then
    if build "srt" "${SRT_VER}"; then
        download "${SRT_URL}" "srt-${CURRENT_PACKAGE_VERSION}.tar.gz"
        cd "$PACKAGES/srt-${CURRENT_PACKAGE_VERSION}" || exit
        export OPENSSL_ROOT_DIR="${WORKSPACE}"
        export OPENSSL_LIB_DIR="${WORKSPACE}"/lib
        export OPENSSL_INCLUDE_DIR="${WORKSPACE}"/include/
        
        STATIC_CPP_LINK="ON"
        if [ "${HOST_NEEDS_MODERN_STD}" = true ]; then
          STATIC_CPP_LINK="OFF"
        fi
        
        execute cmake . \
          -DCMAKE_C_COMPILER=\""${CC}"\" \
          -DCMAKE_CXX_COMPILER=\""${CXX}"\" \
          -DCMAKE_C_FLAGS=\""${CFLAGS}"\" \
          -DCMAKE_CXX_FLAGS=\""${CXXFLAGS}"\" \
          -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" \
          -DCMAKE_INSTALL_LIBDIR=lib \
          -DCMAKE_INSTALL_BINDIR=bin \
          -DCMAKE_INSTALL_INCLUDEDIR=include \
          -DENABLE_SHARED=OFF \
          -DENABLE_STATIC=ON \
          -DENABLE_APPS=OFF \
          -DUSE_STATIC_LIBSTDCXX="${STATIC_CPP_LINK}" \
          -DCMAKE_POLICY_VERSION_MINIMUM="${CMAKE_SYSTEM_MIN_VER}"
        execute make -j $MJOBS install

        if [ -n "$LDEXEFLAGS" ] && [ "$HOST_NEEDS_MODERN_STD" = false ]; then
            sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}"/lib/pkgconfig/srt.pc # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
        fi

        build_done "srt" "${CURRENT_PACKAGE_VERSION}"
    fi
    CONFIGURE_OPTIONS+=("--enable-libsrt")
fi

##
## NVCC Stuffs
##
##
## Hardware Acceleration (CUDA / Jetson / VAAPI / AMF / OpenCL)
##
if [[ "$OSTYPE" != "darwin"* ]]; then

    # 1. NVIDIA CUDA / NVENC / NVDEC (x86_64 dGPU vs. Jetson Tegra)
    if command_exists "nvcc"; then
        if [[ -n "$CUDA_PATH" ]]; then
            CUDA_BASE_PATH="$CUDA_PATH"
        else
            CUDA_BASE_PATH="$(dirname "$(dirname "$(command -v nvcc)")")"
        fi

        CFLAGS+=" -I${CUDA_BASE_PATH}/include"
        if [[ -d "${CUDA_BASE_PATH}/lib64" ]]; then
            LDFLAGS+=" -L${CUDA_BASE_PATH}/lib64"
        elif [[ -d "${CUDA_BASE_PATH}/lib" ]]; then
            LDFLAGS+=" -L${CUDA_BASE_PATH}/lib"
        fi

        # Detect NVIDIA Jetson (Orin Nano = sm_87, Xavier = sm_72, TX2 = sm_62, Nano = sm_53)
        if [[ "$MACHINE_ARCH" == "aarch64" && -f /etc/nv_tegra_release ]]; then
            echo "Detected NVIDIA Jetson platform: configuring CUDA filters without desktop NVENC/NVDEC."
            CUDA_GENCODE="-gencode arch=compute_87,code=sm_87"
            CONFIGURE_OPTIONS+=(
                "--enable-cuda-nvcc"
                "--enable-cuda-llvm"
                "--nvccflags=\"-allow-unsupported-compiler ${CUDA_GENCODE}\""
            )
            if library_exists "libdrm"; then
                CONFIGURE_OPTIONS+=("--enable-libdrm")
            fi
            CONFIGURE_OPTIONS+=("--enable-v4l2-m2m")
        else
            # Desktop / Server GPU (x86_64 or SBSA aarch64)
            if build "nv-codec" "${NV_CODEC_VER}"; then
                download "${NV_CODEC_URL}" "nv-codec-headers-${CURRENT_PACKAGE_VERSION}.tar.gz"
                cd "$PACKAGES/nv-codec-headers-${CURRENT_PACKAGE_VERSION}" || exit
                sed_inplace "s#PREFIX = /usr/local#PREFIX = ${WORKSPACE}#g" Makefile
                execute make install
                build_done "nv-codec" "${CURRENT_PACKAGE_VERSION}"
            fi

            # Select portable gencode based on CUDA major version (CUDA 12+ drops sm_52 in newer toolchains, sm_61/75+ is safer; fallback to sm_52 on older CUDA)
            NVCC_MAJOR="$(get_nvcc_ver | cut -d. -f1)"
            if [[ -n "$NVCC_MAJOR" && "$NVCC_MAJOR" -ge 12 ]]; then
                CUDA_GENCODE="-gencode arch=compute_61,code=sm_61 -gencode arch=compute_75,code=sm_75 -gencode arch=compute_86,code=sm_86"
            else
                CUDA_GENCODE="-gencode arch=compute_52,code=sm_52"
            fi

            CONFIGURE_OPTIONS+=(
                "--enable-ffnvcodec"
                "--enable-cuda-nvcc"
                "--enable-cuda-llvm"
                "--enable-cuvid"
                "--enable-nvdec"
                "--enable-nvenc"
                "--nvccflags=\"-allow-unsupported-compiler ${CUDA_GENCODE}\""
            )

            # libnpp only provides shared libraries
            if [[ "$LDEXEFLAGS" != *"-static"* ]]; then
                CONFIGURE_OPTIONS+=("--enable-libnpp")
            fi
        fi
    fi

    # 2. VAAPI (Intel / AMD on x86_64 Linux) - independent of nvcc
    if [[ "$LDEXEFLAGS" != *"-static"* ]] && library_exists "libva"; then
        if build "vaapi" "system"; then
            build_done "vaapi" "system"
        fi
        CONFIGURE_OPTIONS+=("--enable-vaapi")
    fi

    # 3. AMD AMF (only on x86_64 Linux when AMF_URL is defined in versions.conf)
    if [[ "$MACHINE_ARCH" == "x86_64" && -n "$AMF_URL" && -n "$AMF_VER" ]]; then
        if build "amf" "${AMF_VER}"; then
            download "${AMF_URL}" "AMF-${CURRENT_PACKAGE_VERSION}.tar.gz"
            cd "$PACKAGES/AMF-${CURRENT_PACKAGE_VERSION}" || exit
            execute rm -rf "${WORKSPACE}/include/AMF"
            execute mkdir -p "${WORKSPACE}/include/AMF"
            if [[ -d "amf/public/include" ]]; then
                execute cp -r amf/public/include/* "${WORKSPACE}/include/AMF/"
            elif [[ -d "AMF" ]]; then
                execute cp -r AMF/* "${WORKSPACE}/include/AMF/"
            fi
            build_done "amf" "${CURRENT_PACKAGE_VERSION}"
        fi
        CONFIGURE_OPTIONS+=("--enable-amf")
    fi

    # 4. V4L2 M2M / DRM (Raspberry Pi & ARM SBCs)
    if [[ "$MACHINE_ARCH" == "aarch64" || "$MACHINE_ARCH" == "armv"* ]]; then
        if library_exists "libdrm"; then
            CONFIGURE_OPTIONS+=("--enable-libdrm")
        fi
    fi

    # 5. OpenCL (if clinfo and headers/libs exist)
    if command_exists "clinfo" && library_exists "OpenCL"; then
        CONFIGURE_OPTIONS+=("--enable-opencl")
    fi
fi

if build "ffmpeg" "${FFMPEG_VER}"; then
    download "${FFMPEG_URL}" "ffmpeg-${FFMPEG_VER}.tar.bz2"
    cd "$PACKAGES/ffmpeg-${CURRENT_PACKAGE_VERSION}" || exit

    # Select C++ standard library per OS
    if [[ "$OSTYPE" == "darwin"* ]]; then
        STDLIB_LINK="-lc++"
        FFMPEG_OPTFLAGS="-O3 -fomit-frame-pointer -pipe"
    else
        STDLIB_LINK="-lstdc++"
        FFMPEG_OPTFLAGS="-O3 -fomit-frame-pointer -pipe"
    fi

    # De-duplicate CONFIGURE_OPTIONS (e.g. duplicate --enable-videotoolbox)
    UNIQUE_CONF_OPTS=()
    for opt in "${CONFIGURE_OPTIONS[@]}"; do
        local_dup=false
        for existing in "${UNIQUE_CONF_OPTS[@]}"; do
            if [[ "$opt" == "$existing" ]]; then
                local_dup=true
                break
            fi
        done
        [[ "$local_dup" == false ]] && UNIQUE_CONF_OPTS+=("$opt")
    done

    execute env "${COMPILER_SET}" ./configure \
        "${UNIQUE_CONF_OPTS[@]}" \
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
        --extra-libs=\""${EXTRALIBS} ${STDLIB_LINK}"\" \
        --optflags=\""${FFMPEG_OPTFLAGS}"\" \
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
        install_ffmpeg
    elif [[ ! $SKIPINSTALL == "yes" ]]; then
        read -r -p "Install the binaries to $INSTALL_FOLDER? [Y/n] " response
        case $response in
            [yY][eE][sS] | [yY] | "") install_ffmpeg ;;
        esac
    fi
fi

exit 0
