#!/bin/bash

# https://github.com/markus-perl/ffmpeg-build-script

VERSION=1.17
CWD=$(pwd)
PACKAGES=$CWD/packages
WORKSPACE=$CWD/workspace
CC=clang
CXX=clang++
LDFLAGS="-L${WORKSPACE}/lib -lm -lpthread"
CFLAGS="-I${WORKSPACE}/include -O3 -march=native -pipe -fomit-frame-pointer -fPIE"
CXXFLAGS=$CFLAGS
COMPILER_SET="CC=\"$CC\" CXX=\"$CXX\" CFLAGS=\"$CFLAGS\" CXXFLAGS=\"$CXXFLAGS\" LDFLAGS=\"$LDFLAGS\" "

CONFIGURE_OPTIONS=()

INSTALL_FOLDER="$HOME/.local/bin"
if [[ "$OSTYPE" == "darwin"* ]]; then
	INSTALL_FOLDER="/usr/local/bin"
fi


# Speed up the process
# Env Var NUMJOBS overrides automatic detection
if [[ -n $NUMJOBS ]]; then
    MJOBS=$NUMJOBS
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
		rm -r "$1"
	fi
}

download () {

	DOWNLOAD_PATH=$PACKAGES;

	if [ -n "$3" ]; then
		mkdir -p "$PACKAGES"/"$3"
		DOWNLOAD_PATH=$PACKAGES/$3
	fi;

	if [ ! -f "$DOWNLOAD_PATH/$2" ]; then

		echo "Downloading $1"
		curl -L --silent -o "$DOWNLOAD_PATH/$2" "$1"

		EXITCODE=$?
		if [ $EXITCODE -ne 0 ]; then
			echo ""
			echo "Failed to download $1. Exitcode $EXITCODE. Retrying in 10 seconds";
			sleep 10
			curl -L --silent -o "$DOWNLOAD_PATH/$2" "$1"
		fi

		EXITCODE=$?
		if [ $EXITCODE -ne 0 ]; then
			echo ""
			echo "Failed to download $1. Exitcode $EXITCODE";
			exit 1
		fi

		echo "... Done"

		if ! tar -xvf "$DOWNLOAD_PATH/$2" -C "$DOWNLOAD_PATH" 2>/dev/null >/dev/null; then
			echo "Failed to extract $2";
			exit 1
		fi

	fi
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


build_done () {
    touch "$PACKAGES/$1.done"
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

if build "yasm"; then
	download "https://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz" "yasm-1.3.0.tar.gz"
	cd "$PACKAGES"/yasm-1.3.0 || exit
    execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install
	build_done "yasm"
fi

if build "nasm"; then
	download "https://www.nasm.us/pub/nasm/releasebuilds/2.14.02/nasm-2.14.02.tar.gz" "nasm.tar.gz"
	cd "$PACKAGES"/nasm-2.14.02 || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	build_done "nasm"
fi

if build "opencore"; then
	download "https://deac-riga.dl.sourceforge.net/project/opencore-amr/opencore-amr/opencore-amr-0.1.5.tar.gz" "opencore-amr-0.1.5.tar.gz"
	cd "$PACKAGES"/opencore-amr-0.1.5 || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libopencore_amrnb" "--enable-libopencore_amrwb")
	build_done "opencore"
fi

if build "libvpx"; then
    download "https://github.com/webmproject/libvpx/archive/v1.8.1.tar.gz" "libvpx-1.8.1.tar.gz"
    cd "$PACKAGES"/libvpx-1.8.1 || exit

    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Applying Darwin patch"
        sed "s/,--version-script//g" build/make/Makefile > build/make/Makefile.patched
        sed "s/-Wl,--no-undefined -Wl,-soname/-Wl,-undefined,error -Wl,-install_name/g" build/make/Makefile.patched > build/make/Makefile
    fi

	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-unit-tests --disable-shared
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libvpx")
	build_done "libvpx"
fi

if build "lame"; then
	download "https://netcologne.dl.sourceforge.net/project/lame/lame/3.100/lame-3.100.tar.gz" "lame-3.100.tar.gz"
	cd "$PACKAGES"/lame-3.100 || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libmp3lame")
	build_done "lame"
fi

if build "opus"; then
	download "https://archive.mozilla.org/pub/opus/opus-1.3.1.tar.gz" "opus-1.3.1.tar.gz"
	cd "$PACKAGES"/opus-1.3.1 || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libopus")
	build_done "opus"
fi

if build "xvidcore"; then
	download "https://downloads.xvid.com/downloads/xvidcore-1.3.7.tar.gz" "xvidcore-1.3.7.tar.gz"
	cd "$PACKAGES"/xvidcore  || exit
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

	CONFIGURE_OPTIONS+=("--enable-libxvid")

	build_done "xvidcore"
fi

if build "x264"; then
	download "https://code.videolan.org/videolan/x264/-/archive/stable/x264-stable.tar.bz2" "last_x264.tar.bz2"
	cd "$PACKAGES"/x264-stable || exit

	if [[ "$OSTYPE" == "linux-gnu" ]]; then
		execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic CXXFLAGS="-fPIC"
    else
        execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --enable-static --enable-pic
    fi

    execute make -j $MJOBS
	execute make install
	execute make install-lib-static
	
	CONFIGURE_OPTIONS+=("--enable-libx264")
	build_done "x264"
fi

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
	
	CONFIGURE_OPTIONS+=("--enable-libvorbis")
	build_done "libvorbis"
fi

if build "libtheora"; then
	download "https://ftp.osuosl.org/pub/xiph/releases/theora/libtheora-1.1.1.tar.gz" "libtheora-1.1.1.tar.bz"
	cd "$PACKAGES"/libtheora-1.1.1 || exit
	sed "s/-fforce-addr//g" configure > configure.patched
	chmod +x configure.patched
	mv configure.patched configure
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --with-ogg-libraries="${WORKSPACE}"/lib --with-ogg-includes="${WORKSPACE}"/include/ --with-vorbis-libraries="${WORKSPACE}"/lib --with-vorbis-includes="${WORKSPACE}"/include/ --enable-static --disable-shared --disable-oggtest --disable-vorbistest --disable-examples --disable-asm --disable-spec
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libtheora")
	build_done "libtheora"
fi

if build "pkg-config"; then
	download "https://pkgconfig.freedesktop.org/releases/pkg-config-0.29.2.tar.gz" "pkg-config-0.29.2.tar.gz"
	cd "$PACKAGES"/pkg-config-0.29.2 || exit
	execute env "$COMPILER_SET" ./configure --silent --prefix="${WORKSPACE}" --with-pc-path="${WORKSPACE}"/lib/pkgconfig --with-internal-glib --disable-host-tool
	execute make -j $MJOBS
	execute make install
	build_done "pkg-config"
fi

#if build "cmake"; then
#	download "https://cmake.org/files/v3.15/cmake-3.15.4.tar.gz" "cmake-3.15.4.tar.gz"
#	cd "$PACKAGES"/cmake-3.15.4  || exit
#	rm Modules/FindJava.cmake
#	perl -p -i -e "s/get_filename_component.JNIPATH/#get_filename_component(JNIPATH/g" Tests/CMakeLists.txt
#	perl -p -i -e "s/get_filename_component.JNIPATH/#get_filename_component(JNIPATH/g" Tests/CMakeLists.txt
#	execute env env "$COMPILER_SET" ./bootstrap  --prefix="${WORKSPACE}"
#	execute make -j $MJOBS
#	execute make install
#	build_done "cmake"
#fi

if build "vid_stab"; then
	download "https://github.com/georgmartius/vid.stab/archive/v1.1.0.tar.gz" "georgmartius-vid.stab-v1.1.0-0-g60d65da.tar.tgz"
	cd "$PACKAGES"/vid.stab-1.1.0 || exit
	execute env "$COMPILER_SET" cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DUSE_OMP=OFF -DENABLE_SHARED:bool=off .
	execute make
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libvidstab")
	build_done "vid_stab"
fi

if build "x265"; then
	download "https://github.com/videolan/x265/archive/3.4.tar.gz" "x265-3.4.tar.gz"
	cd "$PACKAGES"/x265-*/ || exit
	cd source || exit
	execute env "$COMPILER_SET" cmake -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DENABLE_SHARED:bool=off .
	execute make -j $MJOBS
	execute make install
	sed "s/-lx265/-lx265 -lstdc++/g" "$WORKSPACE/lib/pkgconfig/x265.pc" > "$WORKSPACE/lib/pkgconfig/x265.pc.tmp"
	mv "$WORKSPACE/lib/pkgconfig/x265.pc.tmp" "$WORKSPACE/lib/pkgconfig/x265.pc"
	
	CONFIGURE_OPTIONS+=("--enable-libx265")
	build_done "x265"
fi

if build "fdk_aac"; then
	download "https://sourceforge.net/projects/opencore-amr/files/fdk-aac/fdk-aac-2.0.1.tar.gz/download?use_mirror=gigenet" "fdk-aac-2.0.1.tar.gz"
	cd "$PACKAGES"/fdk-aac-2.0.1 || exit
	execute env "$COMPILER_SET" ./configure --prefix="${WORKSPACE}" --disable-shared --enable-static
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libfdk-aac")
	build_done "fdk_aac"
fi

if build "av1"; then
	#download "https://aomedia.googlesource.com/aom/+archive/0f5cd05bb3d6209e2583ce682d1acd8e21ae24b8.tar.gz" "av1.tar.gz" "av1"
	git clone https://aomedia.googlesource.com/aom "$PACKAGES"/av1
	cd "$PACKAGES"/av1 || exit
	mkdir -p "$PACKAGES"/aom_build
	cd "$PACKAGES"/aom_build || exit
	execute env "$COMPILER_SET" cmake -DENABLE_TESTS=0 -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" "$PACKAGES"/av1
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-libaom")
	build_done "av1"
fi

if build "zlib"; then
	download "https://www.zlib.net/zlib-1.2.11.tar.gz" "zlib-1.2.11.tar.gz"
	cd "$PACKAGES"/zlib-1.2.11 || exit
	execute env "$COMPILER_SET" ./configure --static --prefix="${WORKSPACE}"
	execute make -j $MJOBS
	execute make install
	build_done "zlib"
fi

if build "openssl"; then
	download "https://www.openssl.org/source/openssl-1.1.1g.tar.gz" "openssl-1.1.1g.tar.gz"
	cd "$PACKAGES"/openssl-1.1.1g || exit
	execute env "$COMPILER_SET" ./config --prefix="${WORKSPACE}" --openssldir="${WORKSPACE}" --with-zlib-include="${WORKSPACE}"/include/ --with-zlib-lib="${WORKSPACE}"/lib no-shared zlib
	execute make -j $MJOBS
	execute make install
	
	CONFIGURE_OPTIONS+=("--enable-openssl")
	build_done "openssl"
fi

if build "srt"; then
	download "https://github.com/Haivision/srt/archive/v1.4.1.tar.gz" "v1.4.1.tar.gz"
	cd "$PACKAGES"/srt-1.4.1 || exit
	export OPENSSL_ROOT_DIR="${WORKSPACE}"
	export OPENSSL_LIB_DIR="${WORKSPACE}"/lib
	export OPENSSL_INCLUDE_DIR="${WORKSPACE}"/include/
	execute cmake "$PACKAGES"/srt-1.4.1 -DCMAKE_INSTALL_PREFIX:PATH="${WORKSPACE}" -DENABLE_SHARED=OFF -DENABLE_STATIC=ON -DENABLE_APPS=OFF
	execute make install

	CONFIGURE_OPTIONS+=("--enable-libsrt")
	build_done "srt"
fi

if command -v nvcc > /dev/null ; then
	if build "nv-codec"; then
		download "https://github.com/FFmpeg/nv-codec-headers/releases/download/n10.0.26.0/nv-codec-headers-10.0.26.0.tar.gz" "nv-codec-headers-10.0.26.0.tar.gz"
		cd "$PACKAGES"/nv-codec-headers-10.0.26.0 || exit
		sed -i  "s#PREFIX = /usr/local#PREFIX = ${WORKSPACE}#g" "$PACKAGES"/nv-codec-headers-10.0.26.0/Makefile
		execute make install
		build_done "nv-codec"
	fi
	CFLAGS+=" -I/usr/local/cuda/include"
	LDFLAGS+=" -L/usr/local/cuda/lib64"
	CONFIGURE_OPTIONS+=("--enable-cuda-nvcc" "--enable-cuvid" "--enable-nvenc" "--enable-libnpp" "--enable-cuda-llvm")
	# https://arnon.dk/matching-sm-architectures-arch-and-gencode-for-various-nvidia-cards/
	CONFIGURE_OPTIONS+=("--nvccflags=-gencode arch=compute_52,code=sm_52")
fi


#CFLAGS="-I$WORKSPACE/include"
#LDFLAGS="-L$WORKSPACE/lib"
#if command -v nvcc > /dev/null ; then
#       if build "nv-codec"; then
#               download "https://github.com/FFmpeg/nv-codec-headers/releases/download/n10.0.26.0/nv-codec-headers-10.0.26.0.tar.gz" "nv-codec-headers-10.0.26.0.tar.gz"
#               cd "$PACKAGES"/nv-codec-headers-10.0.26.0 || exit
#               sed -i  "s#PREFIX = /usr/local#PREFIX = ${WORKSPACE}#g" "$PACKAGES"/nv-codec-headers-10.0.26.0/Makefile
#               execute make install
#               build_done "nv-codec"
#       fi
#       CFLAGS="$CFLAGS -I/usr/local/cuda/include"
#       LDFLAGS="$LDFLAGS -L/usr/local/cuda/lib64"
#       ADDITIONAL_CONFIGURE_OPTIONS="$ADDITIONAL_CONFIGURE_OPTIONS --enable-cuda-nvcc --enable-cuvid --enable-nvenc --enable-libnpp  --enable-cuda-llvm"
#fi

build "ffmpeg"
#download "https://ffmpeg.org/releases/ffmpeg-snapshot.tar.bz2" "ffmpeg-snapshot.tar.bz2"
#cd "$PACKAGES"/ffmpeg/ || exit
git clone https://github.com/FFmpeg/FFmpeg.git "$PACKAGES"/FFMpeg
cd "$PACKAGES"/FFMpeg/ || exit
# shellcheck disable=SC2086

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

sed -z "s/${NVCC_ORIG_TXT}/${NVCC_FIXED_TXT}/g" -i ./configure

PATH="$WORKSPACE/bin:$PATH" \
PKG_CONFIG_PATH="$WORKSPACE/lib/pkgconfig" \
env "$COMPILER_SET" ./configure \
    "${CONFIGURE_OPTIONS[@]}" \
	--disable-debug \
	--disable-doc \
	--disable-ffplay \
	--disable-shared \
	--enable-gpl \
	--enable-nonfree \
	--enable-pthreads \
	--enable-static \
	--enable-small \
	--enable-version3 \
	--extra-cflags="${CFLAGS}" \
	--extra-ldflags="${LDFLAGS}" \
	--extra-libs="${EXTRALIBS}" \
	--pkgconfigdir="$WORKSPACE/lib/pkgconfig" \
	--pkg-config-flags="--static" \
	--prefix="${WORKSPACE}"


execute make -j $MJOBS
execute make install

echo ""
echo "Building done. The binary can be found here: $WORKSPACE/bin/ffmpeg"
echo ""

if [[ $AUTOINSTALL == "yes" ]]; then
	if command_exists "sudo"; then
		sudo cp "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
		sudo cp "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
		echo "Done. ffmpeg is now installed to your system"
	else
		cp "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
		cp "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
		echo "Done. ffmpeg is now installed to your system"
	fi
elif [[ ! $SKIPINSTALL == "yes" ]]; then
	read -r -p "Install the binary to your $INSTALL_FOLDER folder? [Y/n] " response
	case $response in
	[yY][eE][sS]|[yY])
		if command_exists "sudo"; then
			sudo cp "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
			sudo cp "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
		else
			cp "$WORKSPACE/bin/ffmpeg" "$INSTALL_FOLDER/ffmpeg"
			cp "$WORKSPACE/bin/ffprobe" "$INSTALL_FOLDER/ffprobe"
		fi
		echo "Done. ffmpeg is now installed to your system"
		;;
	esac
fi


exit 0