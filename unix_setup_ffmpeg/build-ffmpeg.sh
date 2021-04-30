#!/bin/bash

# Referenced:
# https://github.com/markus-perl/ffmpeg-build-script

VERSION=1.22
CWD=$(pwd -P)
PACKAGES=$CWD/packages
WORKSPACE=$CWD/workspace
CC=clang
CXX=clang++

# Fallback compilers
if [ ! -x "$(command -v clang)" ]; then
  echo "Oops, clang can't be found!! Falling back to system GCC."
  CC="$(command -v gcc)"
  CXX="$(command -v g++)"
fi

# Setting up build environments
LDFLAGS="-L${WORKSPACE}/lib -ldl -lm -lpthread -lz"
LDFLAGS_Z="-L${WORKSPACE}/lib -ldl -lm -lpthread"
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

echo ""
echo "Compilers are..."
echo "C Compiler=${CC}"
echo "C++ Compiler=${CXX}"
echo ""

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

make_dir () {
  if [ ! -d "$1" ]; then
    if ! mkdir "$1"; then
      printf "\n Failed to create dir %s" "$1";
      exit 1
    fi
  fi
}

remove_dir () {
  if [ -d "$1" ]; then
    rm -rfv "$1"
  fi
}

download () {
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
      echo "Failed to download $1. Exitcode $EXITCODE. Retrying in 10 seconds";
      sleep 10
      curl -L --silent -o "$DOWNLOAD_PATH/$DOWNLOAD_FILE" "$1"
    fi

    EXITCODE=$?
    if [ "$EXITCODE" -ne 0 ]; then
      echo ""
      echo "Failed to download $1. Exitcode $EXITCODE";
      exit 1
    fi

    echo "... Done"
  else
    echo "$DOWNLOAD_FILE has already downloaded."
  fi

  make_dir "$DOWNLOAD_PATH/$TARGETDIR"

  if [ -n "$3" ]; then
    if ! tar -xvf "$DOWNLOAD_PATH/$DOWNLOAD_FILE" -C "$DOWNLOAD_PATH/$TARGETDIR" 2>/dev/null >/dev/null; then
      echo "Failed to extract $DOWNLOAD_FILE";
      exit 1
    fi
  else
    if ! tar -xvf "$DOWNLOAD_PATH/$DOWNLOAD_FILE" -C "$DOWNLOAD_PATH/$TARGETDIR" --strip-components 1 2>/dev/null >/dev/null; then
      echo "Failed to extract $DOWNLOAD_FILE";
      exit 1
    fi
  fi

  echo "Extracted $DOWNLOAD_FILE";

  cd "$DOWNLOAD_PATH/$TARGETDIR" || (echo "Error has occurred." ; exit 1)
}

execute () {
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

build () {
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

library_exists () {
  local result=0
  local output=$(pkg-config --exists --print-errors "$1" 2>&1 > /dev/null) || result=$?
  if [ ! "$result" = "0" ]; then
    return 1
  fi

  return 0
}


build_done () {
    touch "$PACKAGES"/"$1.done"
}

cleanup () {
  remove_dir "$PACKAGES"
  remove_dir "$WORKSPACE"
  echo "Cleanup done."
  echo ""
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

vercomp () {
    if [[ "$1" == "$2" ]]
    then
        return 0
    fi
    local IFS=.
    local i ver1=($1) ver2=($2)
    # fill empty fields in ver1 with zeros
    for ((i=${#ver1[@]}; i<${#ver2[@]}; i++))
    do
        ver1[i]=0
    done
    for ((i=0; i<${#ver1[@]}; i++))
    do
        if [[ -z ${ver2[i]} ]]
        then
            # fill empty fields in ver2 with zeros
            ver2[i]=0
        fi
        if ((10#${ver1[i]} > 10#${ver2[i]}))
        then
            return 1
        fi
        if ((10#${ver1[i]} < 10#${ver2[i]}))
        then
            return 2
        fi
    done
    return 0
}

get_nvcc_ver () {
  if [ -v "$(command -v nvcc)" ]; then
    nvcc_ver_txt=$(nvcc --version | grep 'release')
    read -a nvcc_ver_ary <<< "$nvcc_ver_txt"
    nvcc_ver=${${nvcc_ver_txt[-1]}:1}
    echo "$nvcc_ver"
  else
    echo "0.0.0"
  fi
}

nvcc_ver_chk () {
  vercomp $(get_nvcc_ver) $NVCC_VER_THRSH
  case $? in
    0) echo 'Pass';;
    1) echo 'Pass';;
    2) echo 'Fail';;
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

echo "ffmpeg-build-script v$VERSION"
echo "========================="
echo ""

case "$1" in
"--cleanup")
  remove_dir "$PACKAGES"
  remove_dir "$WORKSPACE"
  echo "Cleanup done."
  echo ""
  exit 0
    ;;
"--build")

    ;;
"--full-static")
  if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Error: A full static binary can only be build on Linux."
    exit 1
  fi
  LDEXEFLAGS="-static"
    ;;
*)
    echo "Usage: $0"
    echo "   --build: start building process"
    echo "   --cleanup: remove all working dirs"
    echo "   --help: show this help"
    echo ""
    exit 0
    ;;
esac

echo "Using $MJOBS make jobs simultaneously."

make_dir "$PACKAGES"
make_dir "$WORKSPACE"

export PATH=${WORKSPACE}/bin:$PATH

if ! command_exists "make"; then
    echo "make not installed.";
    exit 1
fi

if ! command_exists "g++"; then
    echo "g++ not installed.";
    exit 1
fi

if ! command_exists "curl"; then
    echo "curl not installed.";
    exit 1
fi

if ! command_exists "cmake"; then
    echo "cmake not installed.";
    exit 1
fi


if [ -n "$LDEXEFLAGS" ]; then
  echo "Start the build in full static mode."
fi

export PATH="${WORKSPACE}/bin:$PATH"
PKG_CONFIG_PATH="/usr/local/lib/x86_64-linux-gnu/pkgconfig:/usr/local/lib/pkgconfig"
PKG_CONFIG_PATH+=":/usr/local/share/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/lib64/pkgconfig"
export PKG_CONFIG_PATH

## Base Libraries to build stuffs
if build "zlib"; then
  download "https://www.zlib.net/zlib-1.2.11.tar.gz" "zlib-1.2.11.tar.gz"
  cd "$PACKAGES"/zlib-1.2.11 || exit
  execute env "$COMPILER_SET_Z" ./configure --static --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "zlib"
fi

if build "pkg-config"; then
  download "https://pkgconfig.freedesktop.org/releases/pkg-config-0.29.2.tar.gz" "pkg-config-0.29.2.tar.gz"
  cd "$PACKAGES"/pkg-config-0.29.2 || exit
  execute env "$COMPILER_SET" ./configure --silent --prefix="${WORKSPACE}" --with-pc-path="${WORKSPACE}"/lib/pkgconfig --with-internal-glib --disable-host-tool
  execute make -j $MJOBS
  execute make install
  build_done "pkg-config"
fi

if build "yasm"; then
  download "https://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz" "yasm-1.3.0.tar.gz"
  cd "$PACKAGES"/yasm-1.3.0 || exit
    execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
  execute make -j $MJOBS
  execute make install
  build_done "yasm"
fi

if build "nasm"; then
  download "https://www.nasm.us/pub/nasm/releasebuilds/2.15.05/nasm-2.15.05.tar.xz" "nasm-2.15.05.tar.xz"
  cd "$PACKAGES"/nasm-2.15.05 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install
  build_done "nasm"
fi

if build "openssl"; then
  download "https://www.openssl.org/source/openssl-1.1.1k.tar.gz" "openssl-1.1.1k.tar.gz"
  cd "$PACKAGES"/openssl-1.1.1k || exit
  execute env "$COMPILER_SET" ./config --prefix="${WORKSPACE}" --openssldir="${WORKSPACE}" --with-zlib-include="${WORKSPACE}"/include/ --with-zlib-lib="${WORKSPACE}"/lib no-shared zlib
  execute make -j $MJOBS
  execute make install_sw

  build_done "openssl"
fi
CONFIGURE_OPTIONS+=("--enable-openssl")

## Media Libraries

# Lv2 crap
if command_exists $PYTHON; then

  if build "lv2"; then
    download "https://lv2plug.in/spec/lv2-1.18.0.tar.bz2" "lv2-1.18.0.tar.bz2"
    execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --lv2-user
    execute $PYTHON ./waf
    execute $PYTHON ./waf install

    build_done "lv2"
  fi

  if build "waflib"; then
    download "https://gitlab.com/drobilla/autowaf/-/archive/cc37724b9bfa889baebd8cb10f38b8c7cab83e37/autowaf-cc37724b9bfa889baebd8cb10f38b8c7cab83e37.tar.gz" "autowaf.tar.gz"
    build_done "waflib"
  fi

  if build "serd"; then
    download "https://gitlab.com/drobilla/serd/-/archive/v0.30.6/serd-v0.30.6.tar.gz" "serd-v0.30.6.tar.gz"
    execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/serd-v0.30.6/waflib/"
    execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared --no-posix
    execute $PYTHON ./waf
    execute $PYTHON ./waf install
    build_done "serd"
  fi

  if build "pcre"; then
    download "https://ftp.pcre.org/pub/pcre/pcre-8.44.tar.gz" "pcre-8.44.tar.gz"
    execute ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
    execute make -j $MJOBS
    execute make install

    build_done "pcre"
  fi

  if build "sord"; then
    download "https://gitlab.com/drobilla/sord/-/archive/v0.16.6/sord-v0.16.6.tar.gz" "sord-v0.16.6.tar.gz"
    execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/sord-v0.16.6/waflib/"
    execute $PYTHON ./waf configure --prefix="${WORKSPACE}" CFLAGS="\"${CFLAGS}\"" --static --no-shared --no-utils
    execute $PYTHON ./waf CFLAGS="\"${CFLAGS}\""
    execute $PYTHON ./waf install

    build_done "sord"
  fi

  if build "sratom"; then
    download "https://gitlab.com/lv2/sratom/-/archive/v0.6.6/sratom-v0.6.6.tar.gz" "sratom-v0.6.6.tar.gz"
    execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/sratom-v0.6.6/waflib/"
    execute $PYTHON ./waf configure --prefix="${WORKSPACE}" --static --no-shared
    execute $PYTHON ./waf
    execute $PYTHON ./waf install

    build_done "sratom"
  fi

  if build "lilv"; then
    download "https://gitlab.com/lv2/lilv/-/archive/v0.24.10/lilv-v0.24.10.tar.gz" "lilv-v0.24.10.tar.gz"
    execute cp -r ${PACKAGES}/autowaf/* "${PACKAGES}/lilv-v0.24.10/waflib/"
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
  download "https://netcologne.dl.sourceforge.net/project/lame/lame/3.100/lame-3.100.tar.gz" "lame-3.100.tar.gz"
  cd "$PACKAGES"/lame-3.100 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install

  build_done "lame"
fi
CONFIGURE_OPTIONS+=("--enable-libmp3lame")

if build "opencore"; then
  download "https://deac-riga.dl.sourceforge.net/project/opencore-amr/opencore-amr/opencore-amr-0.1.5.tar.gz" "opencore-amr-0.1.5.tar.gz"
  cd "$PACKAGES"/opencore-amr-0.1.5 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install

  build_done "opencore"
fi
CONFIGURE_OPTIONS+=("--enable-libopencore_amrnb" "--enable-libopencore_amrwb")

if build "opus"; then
  download "https://archive.mozilla.org/pub/opus/opus-1.3.1.tar.gz" "opus-1.3.1.tar.gz"
  cd "$PACKAGES"/opus-1.3.1 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install

  build_done "opus"
fi
CONFIGURE_OPTIONS+=("--enable-libopus")

if build "libogg"; then
  download "https://ftp.osuosl.org/pub/xiph/releases/ogg/libogg-1.3.3.tar.gz" "libogg-1.3.3.tar.gz"
  cd "$PACKAGES"/libogg-1.3.3 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install
  build_done "libogg"
fi

if build "libvorbis"; then
  download "https://ftp.osuosl.org/pub/xiph/releases/vorbis/libvorbis-1.3.6.tar.gz" "libvorbis-1.3.6.tar.gz"
  cd "$PACKAGES"/libvorbis-1.3.6 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest
  execute make -j $MJOBS
  execute make install

  build_done "libvorbis"
fi
CONFIGURE_OPTIONS+=("--enable-libvorbis")

if build "libtheora"; then
  download "https://ftp.osuosl.org/pub/xiph/releases/theora/libtheora-1.1.1.tar.gz" "libtheora-1.1.1.tar.bz"
  cd "$PACKAGES"/libtheora-1.1.1 || exit
  sed "s/-fforce-addr//g" configure > configure.patched
  chmod +x configure.patched
  mv configure.patched configure
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --with-vorbis-libraries="${WORKSPACE}"/lib --with-vorbis-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest --disable-vorbistest --disable-examples --disable-asm --disable-spec
  execute make -j $MJOBS
  execute make install

  build_done "libtheora"
fi
CONFIGURE_OPTIONS+=("--enable-libtheora")

if build "fdk_aac"; then
  download "https://sourceforge.net/projects/opencore-amr/files/fdk-aac/fdk-aac-2.0.1.tar.gz/download?use_mirror=gigenet" "fdk-aac-2.0.1.tar.gz"
  cd "$PACKAGES"/fdk-aac-2.0.1 || exit
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install

  build_done "fdk_aac"
fi
CONFIGURE_OPTIONS+=("--enable-libfdk-aac")

## Image Library
if build "libwebp"; then
  download "https://github.com/webmproject/libwebp/archive/v1.1.0.tar.gz" "libwebp-1.1.0.tar.gz"
  make_dir "$PACKAGES"/libwebp-1.1.0/build
  cd "$PACKAGES"/libwebp-1.1.0/build || exit
  execute cmake -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_INSTALL_BINDIR=bin -DCMAKE_INSTALL_INCLUDEDIR=include -DENABLE_SHARED=OFF -DENABLE_STATIC=ON ../
  execute make -j $MJOBS
  execute make install

  build_done "libwebp"
fi
CONFIGURE_OPTIONS+=("--enable-libwebp")

if build "libvpx"; then
    download "https://github.com/webmproject/libvpx/archive/v1.9.0.tar.gz" "libvpx-1.9.0.tar.gz"
    cd "$PACKAGES"/libvpx-1.9.0 || exit

    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Applying Darwin patch"
        sed "s/,--version-script//g" build/make/Makefile > build/make/Makefile.patched
        sed "s/-Wl,--no-undefined -Wl,-soname/-Wl,-undefined,error -Wl,-install_name/g" build/make/Makefile.patched > build/make/Makefile
    fi

  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-unit-tests --disable-shared
  execute make -j $MJOBS
  execute make install

  build_done "libvpx"
fi
CONFIGURE_OPTIONS+=("--enable-libvpx")

## Video Library
if build "xvidcore"; then
  download "https://downloads.xvid.com/downloads/xvidcore-1.3.7.tar.gz" "xvidcore-1.3.7.tar.gz"
  cd "$PACKAGES"/xvidcore-1.3.7  || exit
  cd build/generic  || exit
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
  download "https://code.videolan.org/videolan/x264/-/archive/stable/x264-stable.tar.bz2" "x264-stable.tar.bz2"
  cd "$PACKAGES"/x264-stable || exit

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
  download "https://github.com/videolan/x265/archive/Release_3.5.tar.gz" "x265-3.5.tar.gz"
  cd "$PACKAGES"/x265-*/ || exit
  cd source || exit
  execute env "$COMPILER_SET" cmake -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DENABLE_SHARED=off -DBUILD_SHARED_LIBS=OFF .
  execute make -j $MJOBS
  execute make install

  if [ -n "$LDEXEFLAGS" ]; then
    sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}/lib/pkgconfig/x265.pc" # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
  fi

  build_done "x265"
fi
CONFIGURE_OPTIONS+=("--enable-libx265")

if build "vid_stab"; then
  download "https://github.com/georgmartius/vid.stab/archive/v1.1.0.tar.gz" "vid.stab-1.1.0.tar.gz"
  cd "$PACKAGES"/vid.stab-1.1.0 || exit
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
  download "https://www.libsdl.org/release/SDL2-2.0.14.tar.gz"
  execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
  execute make -j $MJOBS
  execute make install

  build_done "libsdl"
fi

if build "srt"; then
  download "https://github.com/Haivision/srt/archive/v1.4.1.tar.gz" "srt-1.4.1.tar.gz"
  cd "$PACKAGES"/srt-1.4.1 || exit
  export OPENSSL_ROOT_DIR="${WORKSPACE}"
  export OPENSSL_LIB_DIR="${WORKSPACE}"/lib
  export OPENSSL_INCLUDE_DIR="${WORKSPACE}"/include/
  execute env "$COMPILER_SET" cmake "$PACKAGES"/srt-1.4.1 -DCMAKE_INSTALL_PREFIX="${WORKSPACE}" -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_INSTALL_BINDIR=bin -DCMAKE_INSTALL_INCLUDEDIR=include -DENABLE_SHARED=OFF -DENABLE_STATIC=ON -DENABLE_APPS=OFF -DUSE_STATIC_LIBSTDCXX=ON
  execute make install

  if [ -n "$LDEXEFLAGS" ]; then
    sed -i.backup 's/-lgcc_s/-lgcc_eh/g' "${WORKSPACE}"/lib/pkgconfig/srt.pc # The -i.backup is intended and required on MacOS: https://stackoverflow.com/questions/5694228/sed-in-place-flag-that-works-both-on-mac-bsd-and-linux
  fi

  build_done "srt"
fi
CONFIGURE_OPTIONS+=("--enable-libsrt")

## NVCC crap
if command -v nvcc > /dev/null ; then
  if "$(nvcc_ver_chk)" == "Pass" ; then
    if build "nv-codec"; then
      download "https://github.com/FFmpeg/nv-codec-headers/releases/download/n11.0.10.0/nv-codec-headers-11.0.10.0.tar.gz" "nv-codec-headers-11.0.10.0.tar.gz"
      cd "$PACKAGES"/nv-codec-headers-11.0.10.0 || exit
      sed -i  "s#PREFIX = /usr/local#PREFIX = ${WORKSPACE}#g" "$PACKAGES"/nv-codec-headers-11.0.10.0/Makefile       execute make install
      build_done "nv-codec"
    fi
    CFLAGS="$CFLAGS -I/usr/local/cuda/include"
    LDFLAGS="$LDFLAGS -L/usr/local/cuda/lib64"
    # CONFIGURE_OPTIONS+=("--nvccflags=-gencode arch=compute_52,code=sm_52")
    CONFIGURE_OPTIONS+=("--enable-cuda-nvcc" "--enable-cuvid" "--enable-nvenc" "--enable-libnpp" "--enable-cuda-llvm")

    if [ -z "$LDEXEFLAGS" ]; then
      CONFIGURE_OPTIONS+=("--enable-libnpp") # Only libnpp cannot be statically linked.
    fi
    
    CONFIGURE_OPTIONS+=("--nvccflags=-gencode arch=compute_52,code=sm_52")
  fi

  if [ -z "$LDEXEFLAGS" ]; then
    if library_exists "libva"; then
      if build "vaapi"; then
        build_done "vaapi"
      fi
      CONFIGURE_OPTIONS+=("--enable-vaapi")
    fi
  fi

fi

build "ffmpeg"
#download "https://ffmpeg.org/releases/ffmpeg-snapshot.tar.bz2" "ffmpeg-snapshot.tar.bz2"
#cd "$PACKAGES"/ffmpeg/ || exit
git clone https://github.com/FFmpeg/FFmpeg.git "$PACKAGES"/FFMpeg
cd "$PACKAGES"/FFMpeg/ || exit
# shellcheck disable=SC2086

if command -v nvcc > /dev/null ; then
  NVCC_ORIG_TXT="$(cat <<-EOF
if enabled cuda_nvcc; then
    nvcc_default="nvcc"
    nvccflags_default="-gencode arch=compute_30,code=sm_30 -O2"
else
    nvcc_default="clang"
    nvccflags_default="--cuda-gpu-arch=sm_30 -O2"
    NVCC_C=""
fi
EOF
)"
  NVCC_ORIG_TXT=${NVCC_ORIG_TXT//$'\n'/\\n}

  NVCC_FIXED_TXT="$(cat <<-EOF
if enabled cuda_nvcc; then
    nvcc_default="nvcc"
    nvccflags_default="-gencode arch=compute_52,code=sm_52 -O2"
else
    nvcc_default="clang"
    nvccflags_default="--cuda-gpu-arch=sm_52 -O2"
    NVCC_C=""
fi
EOF
)"
  NVCC_FIXED_TXT=${NVCC_FIXED_TXT//$'\n'/\\n}

  nvcc_version=$(get_nvcc_ver)
  vercomp "10.0.0" $nvcc_version
  case $? in
    0)
      sed -z "s/${NVCC_ORIG_TXT}/${NVCC_FIXED_TXT}/g" -i ./configure
      ;;
    1)
      ;;
    2)
      sed -z "s/${NVCC_ORIG_TXT}/${NVCC_FIXED_TXT}/g" -i ./configure
      ;;
  esac
fi

PATH="$WORKSPACE/bin:$PATH" \
PKG_CONFIG_PATH="$WORKSPACE/lib/pkgconfig:/usr/local/lib/x86_64-linux-gnu/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/share/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/lib64/pkgconfig" \
env "$COMPILER_SET" ./configure "${CONFIGURE_OPTIONS[@]}" \
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
  --extra-cflags="${CFLAGS}" \
  --extra-ldflags="${LDFLAGS}" \
  --extra-ldexeflags="${LDEXEFLAGS}" \
  --extra-libs="${EXTRALIBS}" \
  --pkgconfigdir="$WORKSPACE/lib/pkgconfig" \
  --pkg-config-flags="--static"
execute make -j $MJOBS
execute make install

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
  [yY][eE][sS]|[yY])
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


exit 0
