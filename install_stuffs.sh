#!/bin/bash
set -e
##########################################################
#                                                        #
# Installing many many stuff script.                     #
#                                                        #
# usage: install_stuff <some directory to brew stuff>    #
#                                                        #
# Written in October 2015 by                             #
#  Taylor Shin                                           #
#                                                        #
##########################################################
if [ "$#" -ne 1 ] ; then
  echo "No homebrew directory has been given!!"
  echo "Checking up \$HOMEBREW"
  if [ -d "$HOMEBREW" ]; then
  	BREWHOME=$HOMEBREW
  	echo "Looks like $HOMEBREW is a valid directory!! Using eet!!! 0.o!"
  else
	echo ""
	echo "usage:"
	echo "    install_stuff <some_homebrew_dir>"
	echo ""
	echo ""
	exit
  fi
else
  echo "Using $1 as homebrew directory!!"
  BREWHOME=$(realpath $1)
fi
CELLAR=$BREWHOME/cellar
PKG_CONFIG_PATH=$BREWHOME/lib/pkgconfig:$PKG_CONFIG_PATH
#LD_LIBRARY_PATH=$BREWHOME/lib:$BREWHOME/lib64
#
# Tarball Download URLs
#
# Programs
URL_GCC="http://mirrors.concertpass.com/gcc/releases/gcc-8.3.0/gcc-8.3.0.tar.xz"
URL_GCC_CUDA="http://mirrors.concertpass.com/gcc/releases/gcc-7.4.0/gcc-7.4.0.tar.xz"
URL_GDB="https://ftp.gnu.org/gnu/gdb/gdb-8.0.tar.xz"
URL_GIT="https://github.com/git/git/archive/v2.15.0.tar.gz"
URL_SQLITE3="https://www.sqlite.org/src/tarball/sqlite.tar.gz"
URL_GPERF="http://ftp.gnu.org/pub/gnu/gperf/gperf-3.0.4.tar.gz"
URL_RUBY="https://github.com/ruby/ruby/archive/v2_4_2.tar.gz"
URL_CURL="https://github.com/bagder/curl/releases/download/curl-7_47_0/curl-7.47.0.tar.gz"
URL_PYTHON3="https://www.python.org/ftp/python/3.7.2/Python-3.7.2.tar.xz"
URL_PYTHON2="https://www.python.org/ftp/python/2.7.15/Python-2.7.15.tar.xz"
URL_PYTHON2_GETPIP="https://bootstrap.pypa.io/get-pip.py"
URL_PERL="http://www.cpan.org/src/5.0/perl-5.26.1.tar.gz"
URL_CMAKE="https://cmake.org/files/v3.9/cmake-3.9.6.tar.gz"
URL_ROOT6="https://root.cern.ch/download/root_v6.06.08.source.tar.gz"
URL_GEANT4="http://geant4.cern.ch/support/source/geant4.10.02.p02.tar.gz"
URL_LXTERMINAL="http://sourceforge.net/projects/lxde/files/LXTerminal (terminal emulator)/LXTerminal 0.2.0/lxterminal-0.2.0.tar.gz"
URL_SIP="http://sourceforge.net/projects/pyqt/files/sip/sip-4.17/sip-4.17.tar.gz"
URL_DBUS="http://dbus.freedesktop.org/releases/dbus/dbus-1.10.6.tar.gz"
URL_LLVM="http://releases.llvm.org/5.0.0/llvm-5.0.0.src.tar.xz"
URL_CLANG="http://releases.llvm.org/5.0.0/cfe-5.0.0.src.tar.xz"
URL_COMPILER_RT="http://releases.llvm.org/5.0.0/compiler-rt-5.0.0.src.tar.xz"
URL_EMACS="http://gnu.mirror.constant.com/emacs/emacs-25.3.tar.xz"
URL_AUTOCONF="ftp://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.xz"
URL_AUTOMAKE="http://ftp.gnu.org/gnu/automake/automake-1.15.tar.xz"
URL_M4="ftp://mirrors.kernel.org/gnu/m4/m4-1.4.17.tar.xz"
URL_GNUPLOT="https://downloads.sourceforge.net/project/gnuplot/gnuplot/5.2.6/gnuplot-5.2.6.tar.gz?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fgnuplot%2Ffiles%2Flatest%2Fdownload&ts=1552334195"
URL_LUA="http://www.lua.org/ftp/lua-5.3.3.tar.gz"
URL_LUA_PATCH="http://www.linuxfromscratch.org/patches/blfs/7.10/lua-5.3.3-shared_library-1.patch"
URL_UNRAR="http://www.rarlab.com/rar/unrarsrc-5.4.5.tar.gz"
URL_YASM="http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz"
URL_ZENITY="http://ftp.gnome.org/pub/GNOME/sources/zenity/2.6/zenity-2.6.2.tar.bz2"
URL_MOUSEPAD="http://archive.xfce.org/src/apps/mousepad/0.4/mousepad-0.4.0.tar.bz2"
URL_DCONF="http://ftp.gnome.org/pub/gnome/sources/dconf/0.26/dconf-0.26.0.tar.xz"
URL_DCONFEDIT="ftp://ftp.gnome.org/pub/gnome/sources/dconf-editor/3.20/dconf-editor-3.20.3.tar.xz"
URL_VIM="https://github.com/vim/vim.git"
URL_VALGRIND="ftp://sourceware.org/pub/valgrind/valgrind-3.13.0.tar.bz2"
URL_VALKYRIE="http://valgrind.org/downloads/valkyrie-2.0.0.tar.bz2"
# Libraries and Modules
URL_LIBFFI="ftp://sourceware.org/pub/libffi/libffi-3.2.1.tar.gz"
URL_GLIB="http://ftp.gnome.org/pub/gnome/sources/glib/2.48/glib-2.48.2.tar.xz"
URL_PCRE="ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.39.tar.bz2"
URL_GSTREAMER_10="http://gstreamer.freedesktop.org/src/gstreamer/gstreamer-1.6.3.tar.xz"
URL_GSTREAMER_01="http://gstreamer.freedesktop.org/src/gstreamer/gstreamer-0.10.36.tar.bz2"
URL_GSTREAMER_PLUGINS_BASE_01="http://gstreamer.freedesktop.org/src/gst-plugins-base/gst-plugins-base-0.10.36.tar.bz2"
URL_GSTREAMER_PLUGINS_BASE_10="http://gstreamer.freedesktop.org/src/gst-plugins-base/gst-plugins-base-1.6.3.tar.xz"
URL_QT_WEBKIT="http://download.kde.org/stable/qtwebkit-2.3/2.3.4/src/qtwebkit-2.3.4.tar.gz"
URL_QT_WEBKIT_PATCH="http://www.linuxfromscratch.org/patches/blfs/7.8/qtwebkit-2.3.4-gcc5-1.patch"
URL_PHONON="http://download.kde.org/stable/phonon/4.8.3/src/phonon-4.8.3.tar.xz"
URL_QT4="https://download.qt.io/archive/qt/4.8/4.8.7/qt-everywhere-opensource-src-4.8.7.tar.gz"
URL_QT5="http://download.qt.io/archive/qt/5.7/5.7.0/single/qt-everywhere-opensource-src-5.7.0.tar.xz"
URL_QT5_WEBKIT="http://www.linuxfromscratch.org/patches/blfs/svn/qt-5.7.0-qtwebengine_glibc224-1.patch"
URL_LIBARCHIVE="http://www.libarchive.org/downloads/libarchive-3.2.1.tar.gz"
URL_LIBEXPAT="http://downloads.sourceforge.net/expat/expat-2.1.0.tar.gz"
URL_VTE="http://ftp.gnome.org/pub/gnome/sources/vte/0.28/vte-0.28.2.tar.xz"
GIT_OPENBLAS="https://github.com/xianyi/OpenBLAS.git"
URL_PYQT4="http://sourceforge.net/projects/pyqt/files/PyQt4/PyQt-4.11.4/PyQt-x11-gpl-4.11.4.tar.gz"
URL_DBUS_GLIB="http://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-0.106.tar.gz"
URL_DBUS_PYTHON="http://dbus.freedesktop.org/releases/dbus-python/dbus-python-1.2.0.tar.gz"
URL_BOOST="https://dl.bintray.com/boostorg/release/1.65.1/source/boost_1_65_1.tar.bz2"
URL_HDF5="http://www.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.0/src/hdf5-1.10.0.tar.bz2"
URL_GIFLIB="http://downloads.sourceforge.net/giflib/giflib-5.1.4.tar.bz2"
URL_XROOTD="http://xrootd.org/download/v4.4.0/xrootd-4.4.0.tar.gz"
URL_GTKSOURCEVIEW="http://ftp.gnome.org/pub/gnome/sources/gtksourceview/2.10/gtksourceview-2.10.5.tar.bz2"
URL_MPICH="http://www.mpich.org/static/downloads/3.2.1/mpich-3.2.1.tar.gz"
URL_ZEROMQ="https://github.com/zeromq/libzmq/releases/download/v4.2.1/zeromq-4.2.1.tar.gz"
# Python Modules
PY_MODULES="pip pexpect pyinstaller sphinx cython autopep8 xlrd xlsxwriter cx_freeze pylint pyopengl pyparsing pillow"
SCI_PY_MODULES="numpy scipy matplotlib pandas pyopengl pyparsing ipython ipywidgets jedi qtconsole sympy cytoolz spyder h5py"
# Some defuault parameters and paths
PROCESSES=$(grep -c ^processor /proc/cpuinfo)
PY3HOME="$BREWHOME/Python3"
PY2HOME="$BREWHOME/Python2"
PYTHON2="$PY2HOME/bin/python2"
PIP2="$PY2HOME/bin/pip2"
PYTHON3="$PY3HOME/bin/python3"
PIP3="$PY3HOME/bin/pip3"
PY2_VER=""
PY3_VER=""
QT4PATH="$BREWHOME/Qt4"
QT4QMAKE="$QT4PATH/bin/qmake"
if [ -e "$BREWHOME/bin/gcc" ]; then
    GCC="$BREWHOME/bin/gcc"
else
    GCC=cc
fi
if [ -e "$BREWHOME/bin/g++" ]; then
    GXX="$BREWHOME/bin/g++"
else
    GXX=c++
fi
if [ -e "$BREWHOME/bin/gfortran" ]; then
    GFORTRAN="$BREWHOME/bin/gfortran"
else
    GFORTRAN=$(command -v gfortran)
fi
GCC_OPTS="-O3 -march=native -fomit-frame-pointer -pipe"
GXX_OPTS=$GCC_OPTS
RPATH="-Wl,-rpath $BREWHOME/lib -Wl,-rpath $BREWHOME/lib64"
GIT="$BREWHOME/bin/git"
CMAKE="$BREWHOME/bin/cmake"
# Default compiler is gcc
COMPILER="gcc"
# Package config path
PKG_CONFIG_PATH=$BREWHOME/lib/pkgconfig:$PKG_CONFIG_PATH
# define default pythin versions.
function get_py_ver {
  if [ -f $BREWHOME/Python2/bin/python2 ] ; then
    PY2VER="$($BREWHOME/Python2/bin/python2 -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')"
    PY2_VER=${PY2VER%.*}
    echo "Python2 detected!! $PY2VER"
    echo "Python2 include dir ver: $PY2_VER"
  fi
  if [ -f $BREWHOME/Python3/bin/python3 ] ; then
    PY3VER="$($BREWHOME/Python3/bin/python3 -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')"
    PY3_VER=${PY3VER%.*}
    echo "Python3 detected!! $PY3VER"
    echo "Python3 include dir ver: $PY3_VER"
  fi
}
# get_distrib_name
# only works properly on redhat or debian(including ubuntu) based distributions...
function get_distrib_name {
  if [ -f /etc/debian_version ] ; then
    echo "Debian"
    return 0
  elif [ -f /etc/redhat-release ] ; then
    echo "RedHat"
    return 0
  else
    echo "Unknown"
    return 0
  fi
}
# Version comparison
#
# Adopted from: http://stackoverflow.com/questions/4023830/bash-how-compare-two-strings-in-version-format
#
# Returns 1 if first is newer, 2 if second is newer. Returns 0 if same.
#
function vercomp () {
  if [[ $1 == $2 ]] ;
  then
    echo 0
  fi
  local IFS=.
  local i ver1=($1) ver2=($2)
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++))
  do
    ver1[i] = 0
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
      echo 1
    fi
    if ((10#${ver1[i]} < 10#${ver2[i]}))
    then
      echo 2
    fi
  done
  echo 0
}
# Select compiler
function set_compiler () {
  if [[ $1 == "gcc" ]] ; then
    if [ ! -f "$BREWHOME/bin/gcc" ]; then
      GCC=$(command -v gcc)
    else
      GCC="$BREWHOME/bin/gcc"
    fi
    if [ ! -f "$BREWHOME/bin/g++" ]; then
      GXX=$(command -v g++)
    else
      GXX="$BREWHOME/bin/g++"
    fi
    if [ ! -f "$BREWHOME/bin/gfortran" ]; then
      GFORTRAN=$(command -v gfortran)
    else
      GFORTRAN="$BREWHOME/bin/gfortran"
    fi
  elif [[ $1 == "llvm" ]] ; then
      GCC="$BREWHOME/bin/clang"
      GCC_OPTS="-O3"
      GXX="$BREWHOME/bin/clang++"
      GXX_OPTS="-O3"
      GFORTRAN=$(command -v gfortran)
  fi
  COMPILER=$1
}
# install compiler
function inst_compiler {
  if [[ $COMPILER == "gcc" ]] ; then
    inst_gcc
  elif [[ $COMPILER == "llvm" ]] ; then
    inst_llvm_clang
  fi
}
# extracts version number
function get_version () {
  INPUT_STR="$1"
  VERSION=$(echo $INPUT_STR | perl -pe '($_)=/([0-9]+([.][0-9]+)+)/' )
  echo $VERSION
}
# extracts program name
function get_prog_name () {
  INPUT_STR="$1"
  BASENAME=$(basename $INPUT_STR)
  PROGNAME=${BASENAME%-*}
  echo $PROGNAME
}
# chk_version
#
# Evaluates version of given command and URL
#
# 1st input: command, 2nd input: URL
#
# returns 0 if current version is up to date. 1 not.
#
function chk_version () {
  VER_CURR_STR=$(command $1 --version)
  VER_CURR_CMD=$(get_version "$VER_CURR_STR")
  VER_CURR_URL=$(get_version $2)
  vercomp $VER_CURR_CMD $VER_CURR_URL
  case $? in
    0) echo 0;;
    1) echo 0;;
    2) echo 1;;
  esac
}
# Check cmd existance
function cmd_exists () {
  command -v $1 > /dev/null 2>&1
}
# GCC Installation
function inst_gcc {
  PROG_NAME=$(get_prog_name $URL_GCC)
  PROG_EXEC=$BREWHOME/bin/gcc
  if cmd_exists $PROG_EXEC ; then
    echo "$PROG_EXEC found!! Checking version"
    chk_version $PROG_EXEC $URL_GCC
    if $? == "0" ; then
      echo "Current $PROG_NAME is up to date!!"
      return 0
    else
      echo "New $PROG_NAME found in URLs!! Installing"
    fi
  fi
  URL=$URL_GCC
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  DIST_NAME=$(get_distrib_name)
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $PROG_NAME $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  echo "Downloading prerequisites for $PROG_NAME $CURR_VER"
  cd $CELLAR/$DIRNAME && ./contrib/download_prerequisites
  echo "Compiling $PROG_NAME $CURR_VER"
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --mandir=$BREWHOME/share/man \
    --infodir=$BREWHOME/share/info \
    --enable-bootstrap \
    --enable-shared \
    --enable-threads=posix \
    --enable-__cxa_atexit \
    --disable-libunwind-exceptions \
    --with-system-zlib \
    --enable-gnu-unique-object \
    --enable-languages=c,c++,objc,obj-c++,fortran \
    --with-arch_32=x86-64 \
    --disable-libmpx \
    --disable-multilib \
    --build=x86_64-redhat-linux \
  && make -j $PROCESSES bootstrap \
  && make install
  mkdir -pv $BREWHOME/share/gdb/auto-load/usr/lib && \
  mv -v $BREWHOME/lib/*gdb.py $BREWHOME/share/gdb/auto-load/usr/lib && \
  chown -v -R kshin:wheel \
    $BREWHOME/lib/gcc/*linux-gnu/$CURR_VER/include{,-fixed}
  ln -svf $BREWHOME/bin/gcc $BREWHOME/bin/cc && \
  install -v -dm755 $BREWHOME/lib/bfd-plugins && \
  ln -svf $BREWHOME/libexec/gcc/$(gcc -dumpmachine)/$CURR_VER/libto_plugin.so $BREWHOME/lib/bfd-plugins
  GCC=$BREWHOME/bin/gcc
  GXX=$BREWHOME/bin/g++
  GFORTRAN=$BREWHOME/bin/gfortran
}
# Some old gcc installation script for CUDA.
function inst_gcc_cuda {
  PROG_NAME=$(get_prog_name $URL_GCC_CUDA)
  PROG_EXEC=$BREWHOME/bin/gcc
  if cmd_exists $PROG_EXEC ; then
    echo "$PROG_EXEC found!! Checking version"
    chk_version $PROG_EXEC $URL_GCC
    if $? == "0" ; then
      echo "Current $PROG_NAME is up to date!!"
      return 0
    else
      echo "New $PROG_NAME found in URLs!! Installing"
    fi
  fi
  URL=$URL_GCC_CUDA
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  DIST_NAME=$(get_distrib_name)

  GCC_CUDA_DIR=$BREWHOME/gcc_cuda
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $PROG_NAME $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  echo "Downloading prerequisites for $PROG_NAME $CURR_VER"
  cd $CELLAR/$DIRNAME && ./contrib/download_prerequisites
  echo "Compiling $PROG_NAME $CURR_VER"
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC="gcc -std=gnu89" CXX="g++ -std=gnu++98" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$GCC_CUDA_DIR \
    --mandir=$GCC_CUDA_DIR/share/man \
    --infodir=$GCC_CUDA_DIR/share/info \
    --enable-bootstrap \
    --enable-shared \
    --enable-threads=posix \
    --enable-checking=release \
    --with-system-zlib \
    --enable-__cxa_atexit \
    --disable-libunwind-exceptions \
    --enable-gnu-unique-object \
    --enable-languages=c,c++,objc,obj-c++,fortran \
    --disable-dssi \
    --disable-multilib \
    --build=x86_64-pc-linux-gnu \
  && make -j $PROCESSES bootstrap \
  && make install
}
# gdb
function inst_gdb {
  PROG_NAME=$(get_prog_name $URL_GDB)
  PROG_EXEC=$BREWHOME/bin/gdb
  if cmd_exists $PROG_EXEC ; then
    echo "$PROG_EXEC found!! Checking version"
    chk_version $PROG_EXEC $URL_GDB
    if $? == 0 ; then
      echo "Current $PROG_NAME is up to date!!"
      return 0
    else
      echo "New $PROG_NAME found in URLs!! Installing"
    fi
  fi
  URL=$URL_GDB
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading GDB $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  echo "Compiling GDB $CURR_VER"
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# Boost
function inst_boost {
  get_py_ver
  TOOLSET="N/A"
  if [[ "$COMPILER" == "gcc" ]] ; then
    TOOLSET="gcc"
  elif [[ "$COMPILER" == "llvm" ]] ; then
    TOOLSET="clang"
  fi
  echo "Using toolset: $TOOLSET"
  URL=$URL_BOOST
  VERSION=$(echo $URL_BOOST | perl -pe '($_)=/([0-9]+([.][0-9]+)+)/' )
  DIR_VERSION=$(echo $VERSION | awk -F '.' '{print $1"_"$2"_"$3}')
  FILENAME="boost.$VERSION.tar.bz2"
  DIRNAME="boost_$DIR_VERSION"
  if [ -d $BREWHOME/include/boost ] ; then
    echo "Boost detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ] ; then
    echo "Downloading Boost ..."
    wget $URL -O $CELLAR/$FILENAME
  else
    echo "$FILENAME has been detected!! Extracting!!"
  fi
  rm -rf $CELLAR/$DIRNAME
  tar xf $CELLAR/$FILENAME -C $CELLAR
  cd $CELLAR/$DIRNAME && \
    sed -e '/using python/ s@;@: ${BREWHOME}/Python3/include/python${PY3_VER}m ;@' \
    -i bootstrap.sh && \
    sed -e '1 i#ifndef Q_MOC_RUN' \
      -e '$ a#endif' \
      -i boost/type_traits/detail/has_binary_operator.hpp && \
    ./bootstrap.sh --prefix=$BREWHOME --with-toolset=$TOOLSET --libdir=$BREWHOME/lib && \
    echo "" >> ./project-config.jam && \
    echo "# for MPI" >> ./project-config.jam && \
    echo "using mpi ; " >> ./project-config.jam && \
    ./b2 stage link=shared threading=multi \
      cxxflags="-I$BREWHOME/Python3/include/python${PY3_VER}m -std=c++98" \
      cflags="-I$BREWHOME/Python3/include/python${PY3_VER}m -std=c++98" && \
    ./b2 install threading=multi link=shared
}
# yasm
function inst_yasm {
  URL=$URL_YASM
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/yasm ; then
    echo "$BREWHOME/bin/yasm found!! Checking version"
    chk_version $BREWHOME/bin/yasm $URL_YASM
    if $? == 0 ; then
      echo "Current YASM is up to date!!"
      return 0
    else
      echo "New YASM found in URLs!! Installing"
    fi
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading YASM $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  cd $CELLAR/$DIRNAME && \
    sed -i 's#) ytasm.*#)#' Makefile.in && \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    ./configure --prefix=$BREWHOME && \
    make && make check && make install
}
# Git
function inst_git {
  URL=$URL_GIT
  CURR_VER=$(get_version $URL)
  FILENAME=git-$CURR_VER.tar.gz
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/git ; then
    echo "$BREWHOME/bin/git found!! Checking version"
    chk_version $BREWHOME/bin/git $URL_GIT
    if [[ $? == 0 ]]; then
      echo "Current GIT is up to date!!"
      return 0
    else
      echo "New GIT found in URLs!! Installing"
    fi
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Git $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  # Gotta delete previous source directory since
  # dedicated build dir doesn't work.
  rm -rfv $CELLAR/$DIRNAME
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  rm -rf $CELLAR/$DIRNAME/configure
  cd $CELLAR/$DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  make configure
  # Building git from dedicated build directory doesn't work...
  cd $CELLAR/$DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="-O3 -L$BREWHOME/lib -L$BREWHOME/lib64 -I$BREWHOME/include" \
  CXXFLAGS="-O3 -L$BREWHOME/lib -L$BREWHOME/lib64 -I$BREWHOME/include" \
  ./configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# LLVM with CLANG
function inst_llvm_clang {
  inst_cmake
  LLVM_FN=$(basename "$URL_LLVM")
  LLVM_VER=$(get_version $LLVM_FN)
  LLVM_DIRNAME=llvm-$LLVM_VER
  CLANG_FN=$(basename "$URL_CLANG")
  CLANG_VER=$(get_version $CLANG_FN)
  CLANG_DIRNAME="clang"
  CRT_FN=$(basename "$URL_COMPILER_RT")
  CRT_VER=$(get_version $CRT_FN)
  CRT_DIRNAME="compiler-rt"
  INST_PREFIX=$BREWHOME
  CMAKE=$BREWHOME/bin/cmake
  PROG_NAME=Clang
  CMD=$BREWHOME/bin/clang
  if cmd_exists $CMD ; then
    echo "$CMD found!! Checking version"
    VER_CHK_RES=$(chk_version $CMD $URL_LLVM)
    if [ "$VER_CHK_RES" == "0" ] ; then
      echo "Current $PROG_NAME is up to date!!"
      return 0
    else
      echo "New $PROG_NAME found in URLs!! Installing"
    fi
  fi
  # LLVM tarball extraction
  if [ ! -f $CELLAR/$LLVM_FN ]; then
    echo "Downloading LLVM $LLVM_VER..."
    wget "$URL_LLVM" -O $CELLAR/$LLVM_FN
  else
    echo "$CELLAR/$LLVM_FN found!!"
  fi
  rm -rf $CELLAR/$LLVM_DIRNAME
  if [ ! -d $CELLAR/$LLVM_DIRNAME ]; then
    echo "Extracting $CELLAR/$LLVM_FN"
    tar xf $CELLAR/$LLVM_FN -C $CELLAR/
    TMP=$(basename "$URL_LLVM")
    mv $CELLAR/${TMP%.*.*} $CELLAR/$LLVM_DIRNAME
  fi
  # Clang tarball extraction
  if [ ! -f $CELLAR/$CLANG_FN ]; then
    echo "Downloading Clang $CLANG_VER..."
    wget "$URL_CLANG" -O $CELLAR/$CLANG_FN
  else
    echo "$CELLAR/$CLANG_FN found!!"
  fi
  if [ ! -d $CELLAR/$LLVM_DIRNAME/tools/$CLANG_DIRNAME ]; then
    echo "Extracting $CELLAR/$CLANG_FN"
    tar xf $CELLAR/$CLANG_FN -C $CELLAR/$LLVM_DIRNAME/tools/
    TMP=$(basename "$URL_CLANG")
    mv $CELLAR/$LLVM_DIRNAME/tools/${TMP%.*.*} \
      $CELLAR/$LLVM_DIRNAME/tools/$CLANG_DIRNAME
  fi
  # compiler-rt tarball extraction
  if [ ! -f $CELLAR/$CRT_FN ]; then
    echo "Downloading compiler-rt $CRT_VER..."
    wget "$URL_COMPILER_RT" -O $CELLAR/$CRT_FN
  else
    echo "$CELLAR/$CRT_FN found!!"
  fi
  if [ ! -d $CELLAR/$LLVM_DIRNAME/projects/$CRT_DIRNAME ]; then
    echo "Extracting $CELLAR/$CRT_FN"
    tar xf $CELLAR/$CRT_FN -C $CELLAR/$LLVM_DIRNAME/projects/
    TMP=$(basename "$URL_COMPILER_RT")
    mv $CELLAR/$LLVM_DIRNAME/projects/${TMP%.*.*} \
      $CELLAR/$LLVM_DIRNAME/projects/$CRT_DIRNAME
  fi
  BLD_DIRNAME=$LLVM_DIRNAME-build
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  # cd $CELLAR/$BLD_DIRNAME && \
  # CC=$BREWHOME/bin/gcc \
  # CXX=$BREWHOME/bin/g++ \
  # CFLAGS="-O3 -L$BREWHOME/lib -L$BREWHOME/lib64 -I$BREWHOME/include" \
  # CXXFLAGS="-O3 -L$BREWHOME/lib -L$BREWHOME/lib64 -I$BREWHOME/include" \
  # $CELLAR/$LLVM_DIRNAME/configure \
  #     --prefix=$INST_PREFIX \
  #     --enable-libffi \
  #     --enable-optimized \
  #     --enable-targets=host,r600 \
  #     --with-python=$BREWHOME/Python2/bin/python2 \
  #     --disable-assertions && \
  # make -j $PROCESSES && \
  # make install
  cd $CELLAR/$BLD_DIRNAME && \
    CC=gcc CXX=g++ \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    $CMAKE $CELLAR/$LLVM_DIRNAME \
         -DCMAKE_INSTALL_PREFIX=$BREWHOME      \
         -DLLVM_ENABLE_FFI=ON                  \
         -DCMAKE_BUILD_TYPE=Release            \
         -DBUILD_SHARED_LIBS=ON                \
         -DLLVM_TARGETS_TO_BUILD="host;AMDGPU" \
         -Wno-dev                           && \
  make -j $PROCESSES && \
  make install
}
# libffi
function inst_libffi {
  URL=$URL_LIBFFI
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib64/libffi.so ]; then
    echo "libffi $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading libffi $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  # Doing some sed stuff
  # Reference: http://www.linuxfromscratch.org/blfs/view/svn/general/libffi.html
  cd $CELLAR/$DIRNAME && \
  sed -e '/^includesdir/ s/$(libdir).*$/$(includedir)/' \
    -i include/Makefile.in && \
  sed -e '/^includedir/ s/=.*$/=@includedir@/' \
    -e 's/^Cflags: -I${includedir}/Cflags:/' \
    -i libffi.pc.in
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# Sqlite3
function inst_sqlite3 {
  URL=$URL_SQLITE3
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/sqlite3 ; then
    echo "$BREWHOME/bin/sqlite3 found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Sqlite3 $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME --disable-static        \
    CFLAGS="-g -O2 -DSQLITE_ENABLE_FTS3=1 \
    -DSQLITE_ENABLE_COLUMN_METADATA=1     \
    -DSQLITE_ENABLE_UNLOCK_NOTIFY=1       \
    -DSQLITE_SECURE_DELETE=1              \
    -DSQLITE_ENABLE_DBSTAT_VTAB=1" \
  && make -j $(($PROCESSES / 2)) \
  && make install
}
# XRootD
function inst_xrootd {
  URL=$URL_XROOTD
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  BIN_FILE=$BREWHOME/bin/xrootd
  if cmd_exists $BIN_FILE ; then
    echo "$BIN_FILE found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading XRootD $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME --disable-static        \
    CFLAGS="-g -O2 -DSQLITE_ENABLE_FTS3=1 \
    -DSQLITE_ENABLE_COLUMN_METADATA=1     \
    -DSQLITE_ENABLE_UNLOCK_NOTIFY=1       \
    -DSQLITE_SECURE_DELETE=1              \
    -DSQLITE_ENABLE_DBSTAT_VTAB=1" \
  && make -j $(($PROCESSES / 2)) \
  && make install
}
# Gnuplot
function inst_gnuplot {
  PROG_NAME=gnuplot
  PROG_EXEC=$BREWHOME/bin/gnuplot
  URL=$URL_GNUPLOT
  FILENAME=gnuplot-5.2.0.tar.gz
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $PROG_NAME $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  echo "Compiling $PROG_NAME $CURR_VER"
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# PCRE
function inst_PCRE {
  URL=$URL_PCRE
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libpcre.so ]; then
    echo "PCRE has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading PCRE $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --docdir=$BREWHOME/share/doc/pcre-$CURR_VER \
    --enable-unicode-properties       \
    --enable-pcre16                   \
    --enable-pcre32                   \
    --enable-pcregrep-libz            \
    --enable-pcregrep-libbz2          \
    --enable-pcretest-libreadline     \
    --disable-static  \
  && make -j $PROCESSES \
  && make install
}
# Glib
function inst_GLib {
  inst_libffi
  inst_PCRE
  URL=$URL_GLIB
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libglib-2.0.so ]; then
    echo "GLib has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading GLib $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-pcre=internal \
  && make -j $PROCESSES \
  && make install
}
# DConf and DConf-Edit
# ==> Compatibility reason
# --> Currently, installs DConf only... will add DConf-Edit if needed later.
#
function inst_dconf_n_dconfedit {
  inst_GLib
  URLDC=$URL_DCONF
  URLDCE=$URL_DCONF_EDIT
  DCFN=$(basename "$URLDC")
  DCEFN=$(basename "$URLDCE")
  CURR_VER_DC=$(get_version $DCFN)
  CURR_VER_DCE=$(get_version $DCEFN)
  DCDN="${DCFN%.*.*}"
  DCEDN="${DCEFN%.*.*}"
  # Download dconf
  if [ -f $BREWHOME/lib/libdconf.so ]; then
    echo "DConf has been found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$DCFN ]; then
    echo "Downloading DConf $CURR_VER_DC..."
    wget "$URLDC" -O $CELLAR/$DCFN
  else
    echo "$CELLAR/$DCFN found!!"
  fi
  if [ ! -f $CELLAR/$DCDN ]; then
    echo "Extracting DConf!!"
    tar xf $CELLAR/$DCFN -C $CELLAR
  fi
  # Let's just skip build-dir generation... not so necessary for this package
  cd $CELLAR/$DCDN && \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DCDN/configure --prefix=$BREWHOME --sysconfdir=$BREWHOME/etc && \
  make && make install
}
# Gstreamer 1.X
function inst_gst10 {
  inst_GLib
  URL=$URL_GSTREAMER_10
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libgstreamer-1.0.so ]; then
    echo "gstreamer-1.0 has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Gstreamer $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-package-name="GStreamer $CURR_VER SUSHI" \
  && make -j $(($PROCESSES / 2)) \
  && make install
}
# Gstreamer 0.1X
function inst_gst01 {
  inst_GLib
  URL=$URL_GSTREAMER_01
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libgstreamer-0.10.so ]; then
    echo "gstreamer-0.10 has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Gstreamer $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-package-name="GStreamer $CURR_VER SUSHI" \
  && make -j &(($PROCESSES / 2)) \
  && make install
}
# Gstreamer plugins base 0.1X
function inst_gst_plugins_base01 {
  inst_gst01
  URL=$URL_GSTREAMER_PLUGINS_BASE_01
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libgstbase-0.10.so ]; then
    echo "gstreamer-base-0.10 has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Gstreamer-plugins-base $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-package-name="GStreamer Base Plugins $CURR_VER SUSHI" \
  && make -j $PROCESSES \
  && make install
}
# Gstreamer plugins base 1.X
function inst_gst_plugins_base10 {
  inst_gst10
  URL=$URL_GSTREAMER_PLUGINS_BASE_10
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libgstbase-1.0.so ]; then
    echo "gstreamer-base-1.0 has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Gstreamer-plugins-base $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-package-name="GStreamer Base Plugins $CURR_VER SUSHI" \
  && make -j $PROCESSES \
  && make install
}
# gperf
function inst_gperf {
  URL=$URL_GPERF
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/gperf ; then
    echo "$BREWHOME/bin/gperf found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading gperf $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# Ruby
function inst_ruby {
  URL=$URL_RUBY
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/ruby ; then
    echo "$BREWHOME/bin/ruby found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading ruby $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --enable-shared \
  && make -j $PROCESSES \
  && make install
}
# Perl
function inst_perl {
  URL=$URL_PERL
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/perl ; then
    echo "$BREWHOME/bin/perl found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading perl $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  cd $CELLAR/$DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  sh $CELLAR/$DIRNAME/Configure \
    -des \
    -Dprefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# Qt-webkit
function inst_qtwebkit {
  inst_gst_plugins_base10
  inst_gperf
  inst_ruby
  inst_python2
  if [[ $COMPILER == "llvm" ]] ; then
    set_compiler "gcc"
  fi
  URL=$URL_QT_WEBKIT
  PATCHURL=$URL_QT_WEBKIT_PATCH
  FILENAME=$(basename "$URL")
  PATCHFILENAME=$(basename "$PATCHURL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Qt-Webkit $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    rm -rf $CELLAR/$DIRNAME && mkdir $CELLAR/$DIRNAME
    tar xf $CELLAR/$FILENAME -C $CELLAR/$DIRNAME
  fi
  # Patching sourcefile for GCC 5.X
  wget "$PATCHURL" -O $CELLAR/$PATCHFILENAME
  cd $CELLAR/$DIRNAME && \
  patch -Np1 -i $CELLAR/$PATCHFILENAME
  cd $CELLAR/$DIRNAME && \
  QTDIR=$QT4PATH \
  PATH=$BREWHOME/Python2/bin:$QT4PATH/bin:$PATH \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/Tools/Scripts/build-webkit \
    --qt \
    --no-webkit2 \
    --prefix=$QT4PATH \
    --makeargs=-j$PROCESSES && \
  make -C $CELLAR/$DIRNAME/WebKitBuild/Release install
  if [[ $COMPILER == "llvm" ]] ; then
    set_compiler $COMPILER
  fi
}
# Phonon
function inst_phonon {
  inst_cmake
  inst_GLib
  URL=$URL_PHONON
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $QT4PATH/lib/libphonon.so ]; then
    echo "Phonon has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Phonon $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  rm -rf ./* && \
  CFLAGS="$CFLAGS $GCC_OPTS" \
  CXXFLAGS="$CXXFLAGS -std=c++98 $GXX_OPTS" \
  $CMAKE -G "Unix Makefiles" \
    $CELLAR/$DIRNAME \
    -DCMAKE_C_COMPILER=$GCC \
    -DCMAKE_CXX_COMPILER=$GXX \
    -DCMAKE_INSTALL_PREFIX=$QT4PATH \
    -DPHONON_INSTALL_QT_EXTENSIONS_INTO_SYSTEM_QT=TURE \
    -DCMAKE_BUILD_TYPE="Release" \
    -DCMAKE_PREFIX_PATH=$QT4PATH \
    -DQT_QMAKE_EXECUTABLE=$QT4PATH/bin/qmake \
    -Wno-dev \
  && make -j $PROCESSES \
  && make install
}
# Qt 4.8.7
function inst_qt4 {
  if cmd_exists $QT4PATH/bin/qmake ; then
    echo "$QT4PATH/bin/qmake found!!"
    return 0
  fi
  if [[ $COMPILER == "llvm" ]] ; then
    set_compiler "gcc"
  fi
  #inst_gst_plugins_base01
  #inst_gst_plugins_base10
  URL=$URL_QT4
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Qt $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  # Patching weird error.
  rm -rf $INST_DIR
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME

  cd $CELLAR/$BLD_DIRNAME && \
  rm -rf ./* && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  PKG_CONFIG_PATH=$PKG_CONFIG_PATH \
  CFLAGS="$CFLAGS" \
  CXXFLAGS="$CXXFLAGS -std=gnu++98" \
  LDFLAGS=$RPATH \
  $CELLAR/$DIRNAME/configure -v \
    -prefix $QT4PATH \
    -confirm-license \
    -opensource \
    -release \
    -nomake demos \
    -nomake examples \
    -nomake docs \
    -optimized-qmake \
    -no-webkit \
    -no-nis \
    -system-sqlite \
    -R $BREWHOME/lib \
    -R $BREWHOME/lib64 \
  && make -j $PROCESSES \
  && make install && \
  rm -rfv $QT4PATH/tests
  find $QT4PATH/lib/pkgconfig -name "*.pc" -exec perl -pi -e "s, -L$PWD/?\S+,,g" {} \;
  for file in $QT4PATH/lib/libQt*.prl; do
    sed -r -e '/^QMAKE_PRL_BUILD_DIR/d' \
      -e 's/(QMAKE_PRL_LIBS =).*/\1/' \
      -i $file
  done
  QT4QMAKE="$QT4PATH/bin/qmake LDFLAGS=$RPATH"
  inst_phonon
  inst_qtwebkit
  if [[ $COMPILER == "llvm" ]] ; then
    set_compiler $COMPILER
  fi
}
# Qt5.7.0
#
# Kinda deprecated since Qt5 provides pretty hefty installer.
#
function inst_qt5 {
  if cmd_exists $QT5PATH/bin/qmake ; then
    echo "$IQT4PATH/bin/qmake found!!"
    return 0
  fi
  if [[ $COMPILER == "llvm" ]] ; then
    set_compiler "gcc"
  fi
  inst_gst_plugins_base01
  inst_gst_plugins_base10
  URL=$URL_QT4
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Qt $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  # Patching weird error.
  rm -rf $INST_DIR
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  rm -rf ./* && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  PKG_CONFIG_PATH=$PKG_CONFIG_PATH \
  $CELLAR/$DIRNAME/configure -v \
    -prefix $QT4PATH \
    -confirm-license \
    -opensource \
    -release \
    -nomake demos \
    -nomake examples \
    -nomake docs \
    -optimized-qmake \
    -no-webkit \
    -no-nis \
    -no-phonon \
    -no-phonon-backend \
    -system-sqlite \
  && make -j $PROCESSES \
  && make install && \
  rm -rfv $QT4PATH/tests
  find $QT4PATH/lib/pkgconfig -name "*.pc" -exec perl -pi -e "s, -L$PWD/?\S+,,g" {} \;
  for file in $QT4PATH/lib/libQt*.prl; do
    sed -r -e '/^QMAKE_PRL_BUILD_DIR/d' \
      -e 's/(QMAKE_PRL_LIBS =).*/\1/' \
      -i $file
  done
  QT4QMAKE="$QT4PATH/bin/qmake LDFLAGS=$RPATH"
  inst_phonon
  inst_qtwebkit
  if [[ $COMPILER == "llvm" ]] ; then
    set_compiler $COMPILER
  fi
}
# cURL
function inst_curl {
  URL=$URL_CURL
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libcurl.so ]; then
    echo "cURL $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading cURL $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --disable-static \
    --enable-threaded-resolver \
  && make -j $PROCESSES \
  && make install
}
# libarchive
function inst_libarchive {
  inst_expat
  URL=$URL_LIBARCHIVE
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libarchive.so ]; then
    echo "libarchive has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading LibArchive $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  # Applying patch
  # cd $CELLAR/$DIRNAME && \
  #     patch -Npl -i ../libarchive-3.1.2-upstream_fixes-1.patch
  # Running installation
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --disable-static \
  && make \
  && make install
}
function inst_lua {
  URL=$URL_LUA
  PATCH_URL=$URL_LUA_PATCH
  FILENAME=$(basename "$URL")
  PATCH_FILENAME=$(basename "$PATCH_URL")
  CURR_VER=$(get_version $FILENAME)
  LUA_VER=${CURR_VER%.*}
  DIRNAME="${FILENAME%.*.*}"
  INST_DIR=$BREWHOME
  if cmd_exists $INST_DIR/bin/lua ; then
    echo "$INST_DIR/bin/lua found!!"
    return 0
  fi
  # Download lua source file and patch
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Lua $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -f $CELLAR/$PATCH_FILENAME ]; then
    echo "Downloading Lua Patch for $CURR_VER..."
    wget "$PATCH_URL" -O $CELLAR/$PATCH_FILENAME
  else
    echo "$CELLAR/$PATCH_FILENAME found!!"
  fi
  # And uncompress it!!
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  # Ok, we need to apply the patch first
  cd $CELLAR/$DIRNAME && \
    patch -Np1 -i $CELLAR/$PATCH_FILENAME && \
    sed -i '/#define LUA_ROOT/s:'"$BREWHOME"':/' src/luaconf.h
  # Then compile & install it
  cd $CELLAR/$DIRNAME && \
    make MYCFLAGS="-DLUA_COMPAT_5_2 -DLUA_COMPAT_5_1" linux && \
    make INSTALL_TOP=$BREWHOME TO_LIB="liblua.so liblua.so.$LUA_VER liblua.so.$CURR_VER" INSTALL_DATA="cp -d" INSTALL_MAN=$BREWHOME/share/man/man1 install && \
    mkdir -pv $BREWHOME/share/doc/lua-$CURR_VER
  # finally, create pkgconfig file and install it
  cat > lua.pc << "EOF"
V=$LUA_VER
R=$CURR_VER
lua_prefix=$BREWHOME
INSTALL_BIN=${lua_prefix}/bin
INSTALL_INC=${lua_prefix}/include
INSTALL_LIB=${lua_prefix}/lib
INSTALL_MAN=${lua_prefix}/share/man/man1
INSTALL_LMOD=${lua_prefix}/share/lua/${V}
INSTALL_CMOD=${lua_prefix}/lib/lua/${V}
exec_prefix=${lua_prefix}
libdir=${exec_prefix}/lib
includedir=${lua_prefix}/include
Name: Lua
Description: An Extensible Extension Language
Version: ${R}
Requires:
Libs: -L${libdir} -llua -lm
Cflags: -I${includedir}
EOF
  install -v -m644 -D lua.pc $BREWHOME/lib/pkgconfig/lua.pc
}
# Python3
function inst_python3 {
  inst_libffi
  inst_expat
  URL=$URL_PYTHON3
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  PY3_VER=${CURR_VER%.*}
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  INST_DIR=$BREWHOME/Python$CURR_VER
  if cmd_exists $INST_DIR/bin/python3 ; then
    echo "$INST_DIR/bin/python3 found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Python $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC \
  CFLAGS="$GCC_OPTS -L$BREWHOME/lib -L$BREWHOME/lib64 -I$BREWHOME/include" \
  LDFLAGS="$RPATH -Wl,-rpath=$INST_DIR/lib" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$INST_DIR \
    --enable-shared \
    --with-system-expat \
    --with-system-ffi \
    --enable-ipv6 \
    && make -j $PROCESSES \
    && make install
  ln -svf $INST_DIR $INST_DIR/../Python3
  PYTHON3=$INST_DIR/bin/python3
  PIP3=$INST_DIR/bin/pip3
}
# Python2
function inst_python2 {
  inst_libffi
  inst_expat
  URL=$URL_PYTHON2
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  PY3_VER=${CURR_VER%.*}
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  INST_DIR=$BREWHOME/Python$CURR_VER
  if cmd_exists $INST_DIR/bin/python2 ; then
    echo "$INST_DIR/bin/python2 found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Python $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC \
  CFLAGS="$GCC_OPTS -L$BREWHOME/lib -L$BREWHOME/lib64 -I$BREWHOME/include" \
  LDFLAGS="$RPATH -Wl,-rpath=$INST_DIR/lib" \
  $CELLAR/$DIRNAME/configure \
  --prefix=$INST_DIR \
  --enable-shared \
  --with-system-expat \
  --with-system-ffi \
  --enable-ipv6 \
  --enable-unicode=ucs4 \
  && make -j $PROCESSES \
  && make install
  ln -svf $INST_DIR $INST_DIR/../Python2
  ln -svf $INST_DIR/bin/python2 $BREWHOME/bin/python2
  ln -svf $INST_DIR/bin/pip2 $BREWHOME/bin/pip2
  PYTHON2=$INST_DIR/bin/python2
  # Installing pip for Python2
  cd $CELLAR && \
  wget $URL_PYTHON2_GETPIP \
    -O $CELLAR/$BLD_DIRNAME/get-pip.py && \
  $PYTHON2 $CELLAR/$BLD_DIRNAME/get-pip.py
  PIP2=$INST_DIR/bin/pip2
}
# expat
function inst_expat {
  URL=$URL_LIBEXPAT
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libexpat.so ] ; then
    echo "libexpat $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ] ; then
    echo "Downloading expat $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ] ; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xzf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --disable-static \
  && make -j $PROCESSES \
  && make install && \
  install -v -m755 -d $BREWHOME/share/doc/expat-$CURR_VER && \
  install -v -m644 doc/*.{html,png,css} $BREWHOME/share/doc/expat-$CURR_VER
}
# giflib
function inst_giflib {
  URL=$URL_GIFLIB
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/bin/gif2rgb ] ; then
    echo "libexpat $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ] ; then
    echo "Downloading expat $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ] ; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --disable-static \
  && make -j $PROCESSES \
  && make install && \
  install -v -m755 -d $BREWHOME/share/doc/expat-$CURR_VER && \
  install -v -m644 doc/*.{html,png,css} $BREWHOME/share/doc/expat-$CURR_VER
}
# cmake
function inst_cmake {
#    inst_qt4
  inst_curl
  inst_libarchive
  inst_expat
  inst_python3
  $PIP3 install -U sphinx
  INST_DIR=$BREWHOME
  PROG_NAME=cmake
  PROG_EXEC=$INST_DIR/bin/cmake
  if cmd_exists $PROG_EXEC ; then
    echo "$PROG_EXEC found!! checking version"
    chk_version $PROG_EXEC $(basename "$URL_CMAKE")
    if [ "$?" == "0" ] ; then
      echo "$PROG_NAME is up to date!!"
      return 0
    fi
  fi
  URL=$URL_CMAKE
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $PROG_NAME $CURR_VER ..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  LDFLAGS="$RPATH -L$QT4PATH/lib" \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/bootstrap \
    --prefix=$INST_DIR \
    --qt-gui \
    --qt-qmake=$QT4QMAKE \
    --no-system-jsoncpp \
    -- -DCMAKE_PREFIX_PATH=$QT4PATH \
  && make -j $PROCESSES \
  && make install
  CMAKE=$INST_DIR/bin/cmake
}
# ROOT 6
function inst_ROOT6 {
  inst_cmake
  inst_python2
  URL=$URL_ROOT6
  FILENAME=$(basename "$URL")
  CURR_VER=$(echo "${FILENAME%.*.*.*}" | awk -F"_v" '{print $NF}')
  DIRNAME=root-$CURR_VER
  BLD_DIRNAME=$DIRNAME-build
  HOME_CMAKE=$BREWHOME/bin/cmake
  INST_DIR="$BREWHOME/root"
  PROG_NAME=ROOT6
  PROG_EXEC="$BREWHOME/root/bin/root"
  if cmd_exists "$PROG_EXEC" ; then
    echo "$PROG_EXEC found!! checking version"
    CURR_VER=$(echo command $PROG_EXEC --version)
    VER_CMP_RES=$(vercomp $CURR_VER $URL_CMAKE)
    if [ "$VER_CMP_RES" == "0" ] ; then
      echo "$PROG_NAME is up to date!!"
      return 0
    fi
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $PROG_NAME $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  $HOME_CMAKE -G "Unix Makefiles" \
    $CELLAR/$DIRNAME \
    -DCMAKE_C_COMPILER=$GCC \
    -DCMAKE_CXX_COMPILER=$GXX \
    -DCMAKE_INSTALL_PREFIX=$INST_DIR \
    -DCMAKE_INCLUDE_PATH=$QT4PATH/include \
    -DCMAKE_LIBRARY_PATH=$QT4PATH/lib \
    -DCMAKE_BUILD_TYPE="Release" \
    -DCMAKE_C_FLAGS="-O3 -fPIC $GCC_OPTS" \
    -DCMAKE_CXX_FLAGS="-O3 -fPIC $GXX_OPS -D_GLIBCXX_USE_CXX11_ABI=0" \
    -DPYTHON_EXECUTABLE="$PYTHON2" \
    -DCMAKE_PREFIX_PATH=$QT4PATH \
    -Dbuiltin_fftw3:bool=ON \
    -Dbuiltin_gsl:bool=ON \
    -Dbuiltin_freetype:bool=ON \
    -Dbuiltin_zlib:bool=ON \
    -Dbuiltin_lzma:bool=ON \
    -Dbuiltin_cfitsio:bool=ON \
    -Dbuiltin_davix:bool=ON \
  && make -j 8 \
  && make install
}
# Geant4.10.02
function inst_Geant4 {
  inst_expat
  inst_cmake
#    inst_qt4
  inst_python2
  URL=$URL_GEANT4
  FILENAME=$(basename "$URL")
  CURR_VER=$(get_version $FILENAME)
  DIRNAME="${FILENAME%.*.*}"
  BLD_DIRNAME=$DIRNAME-build
  INST_DIR=$BREWHOME/Geant4
  HOME_CMAKE=$CMAKE
  PROG_NAME=Geant4
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $PROG_NAME $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  $HOME_CMAKE -G "Unix Makefiles" \
    $CELLAR/$DIRNAME \
    -DCMAKE_C_COMPILER=$GCC \
    -DCMAKE_CXX_COMPILER=$GXX \
    -DCMAKE_INSTALL_PREFIX=$INST_DIR \
    -DCMAKE_INCLUDE_PATH=$QT4PATH/include:$BREWHOME/include \
    -DCMAKE_LIBRARY_PATH=$QT4PATH/lib:$BREWHOME/lib:$BREWHOME/lib64 \
    -DCMAKE_BUILD_TYPE="Release" \
    -DGEANT4_INSTALL_DATA=ON \
    -DGEANT4_BUILD_MULTIGHREADED=ON \
    -DGEANT4_USE_QT=ON \
    -DQT_QMAKE_EXECUTABLE=$QT4QMAKE \
    -DCMAKE_PREFIX_PATH=$QT4PATH \
    -DGEANT4_USE_OPENGL_X11=ON \
  && make -j $PROCESSES VERBOSE=1 \
  && make install
}
# VTE 0.2X
function inst_vte {
  URL=$URL_VTE
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libvte.so ]; then
    echo "libvte $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading VTE $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-gtk=2.0 \
    --libexecdir=$BREWHOME/lib/vte \
    --enable-shared \
  && make -j $PROCESSES \
  && make install
}
# LxTerminal
function inst_lxterminal {
  inst_vte
  URL=$URL_LXTERMINAL
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/lxterminal ; then
    echo "$BREWHOME/bin/lxterminal found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading LxTerminal $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  LDFLAGS=$RPATH \
  vte_CFLAGS="-I$BREWHOME/include/vte-0.0 -lvte" \
  vte_LIBS="-L$BREWHOME/lib" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# OpenBLAS
function inst_openblas () {
  inst_git
  PYTHON=$1
  PYDIR=$(dirname $(command -v $PYTHON))/../
  PYVER="$($PYTHON -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')"
  if [ -f $PYDIR/../lib/libopenblas.so ]; then
    echo "OpenBLAS $CURR_VER has been detected!!"
    return 0
  fi
  URL=$GIT_OPENBLAS
  #FILENAME=$(basename "$URL")
  #DIRNAME="${FILENAME%.*.*}"
  FILENAME="OpenBLAS"
  DIRNAME=$FILENAME-Py-$PYVER
  #CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  rm -rf $CELLAR/$DIRNAME
  if [ ! -f $CELLAR/$DIRNAME ]; then
    echo "Checking out OpenBlas from GitHub..."
    #wget "$URL" -O $CELLAR/$FILENAME
    cd $CELLAR && \
    $GIT clone $URL $DIRNAME
  else
    echo "$CELLAR/$DIRNAME found!!"
  fi
  cd $CELLAR/$DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  make FC=$GFORTRAN
  cd $CELLAR/$DIRNAME && \
  make PREFIX=$PYDIR install
}
# Sip
function inst_sip () {
  PYTHON=$1
  PYVER="$($PYTHON -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')"
  PYVER_SHORT="$($PYTHON -c 'import sys; print(".".join(map(str, sys.version_info[:1])))')"
  URL=$URL_SIP
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build-$PYVER
  if cmd_exists $(dirname $(command -v $PYTHON))/sip ; then
    echo "SIP found for Python$PYVER_SHORT!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading SIP $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $PYTHON $CELLAR/$DIRNAME/configure.py \
  && make -j$PROCESSES \
  && make install
}
# dbus
function inst_dbus {
  inst_GLib
  URL=$URL_DBUS
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/dbus-daemon ; then
    echo "$BREWHOME/bin/dbus-daemon found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading DBUS $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ ! -d $CELLAR/$BLD_DIRNAME ]; then
    mkdir $CELLAR/$BLD_DIRNAME
  else
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
    mkdir $CELLAR/$BLD_DIRNAME
  fi
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --disable-static \
  && make -j $PROCESSES \
  && make install
}
# dbus-GLib
function inst_dbus-glib {
  URL=$URL_DBUS_GLIB
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f $BREWHOME/lib/libdbus-glib-1.so ]; then
    echo "DBUS-GLib $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading DBUS-GLib $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --disable-static \
  && make -j $PROCESSES \
  && make install
}
# dbus-python
function inst_dbus-python () {
  inst_dbus
  inst_dbus-glib
  PYTHON=$1
  PYVER="$($PYTHON -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')"
  PYVER_SHORT="$($PYTHON -c 'import sys; print(".".join(map(str, sys.version_info[:1])))')"
  PYDIR=$(dirname $(command -v $PYTHON))
  URL=$URL_DBUS_PYTHON
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build-$PYVER
  EXIST=$($PYTHON -c 'import pkgutil; print(1 if pkgutil.find_loader("dbus") else 0)')
  if [[ $EXIST == "1" ]]; then
    echo "DBUS-Python seems to be installed already."
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading DBUS $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  PYTHON=$PYTHON \
  PKG_CONFIG_PATH=$BREWHOME/lib/pkgconfig:$PKG_CONFIG_PATH \
  $CELLAR/$DIRNAME/configure --prefix=$PYDIR/.. \
  && make -j$PROCESSES \
  && make install
}
# PyQt
function inst_pyqt4 () {
  PYTHON=$1
  #inst_dbus-python $PYTHON
  inst_sip $PYTHON
  EXIST=$($PYTHON -c 'import pkgutil; print(1 if pkgutil.find_loader("PyQt4") else 0)')
  if [[ $EXIST == "1" ]] ; then
    echo "PyQt4 seems to be installed already."
    return 0
  fi
  PYVER="$($PYTHON -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')"
  URL=$URL_PYQT4
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build-$PYVER
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Sip $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  echo CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS -Wl,-rpath=$(dirname $(command -v $PYTHON))/../lib -I$(dirname $(command -v $PYTHON))/../include" \
  CXXFLAGS="$GXX_OPTS -Wl,-rpath=$(dirname $(command -v $PYTHON))/../lib -I$(dirname $(command -v $PYTHON))/../include" \
  $PYTHON $CELLAR/$DIRNAME/configure.py \
  --qmake=$QT4QMAKE \
  --confirm-license \
  --sip=$(dirname $(command -v $PYTHON))/sip
  cd $CELLAR/$BLD_DIRNAME && \
  rm -rf ./* && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS -L$(dirname $(command -v $PYTHON))/../lib -I$(dirname $(command -v $PYTHON))/../include" \
  CXXFLAGS="$GXX_OPTS -L$(dirname $(command -v $PYTHON))/../lib -I$(dirname $(command -v $PYTHON))/../include" \
  $PYTHON $CELLAR/$DIRNAME/configure.py \
  --qmake=$QT4QMAKE \
  --confirm-license \
  --sip=$(dirname $(command -v $PYTHON))/sip \
  && make CC="$GCC -std=c90" CXX="$GXX -std=c++98" -j$PROCESSES \
  && make install
}
# autoconf
function inst_autoconf {
  URL=$URL_AUTOCONF
  TITLE="Autoconf"
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f cmd_exists $BREWHOME/bin/autoconf ]; then
    echo "$TITLE $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading autoconf $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make -j $PROCESSES \
  && make install
}
# automake
function inst_automake {
  URL=$URL_AUTOMAKE
  TITLE="Automake"
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f cmd_exists $BREWHOME/bin/aclocal ]; then
    echo "$TITLE $CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $Title $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  # Fixing perl-5.22 warning
  cd $CELLAR/DIRNAME && \
  sed -i 's:/\\\${:/\\\$\\{:' bin/automake.in
  CC="$GCC" CXX="$GXX" \
  cd $CELLAR/$DIRNAME && \
  chmod +x ./bootstrap.sh && \
  ./bootstrap.sh
  # Now install it!
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --docdir=$BREWHOME/share/doc/automake-$CURR_VER \
  && make \
  && cd $CELLAR/$DIRNAME \
  && sed -i "s:./configure:LEXLIB=/usr/lib/libfl.a &:" t/lex-{clean,depend}-cxx.sh \
  && cd $CELLAR/$BLD_DIRNAME \
  && make check \
  && make install
}
# valgrind
function inst_valgrind {
  URL=$URL_VALGRIND
  TITLE="Valgrind"
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build

  if [ -f cmd_exists $BREWHOME/bin/valgrind ]; then
    echo "$TITLE $CURR_VER has been detected!!"
    return 0
  fi

  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $Title $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME

  # Now install it!
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make \
  && make install
}
# valkyrie
function inst_valkyrie {
  URL=$URL_VALKYRIE
  TITLE="Valkyrie"
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build

  if [ -f cmd_exists $BREWHOME/bin/valkyrie ]; then
    echo "$TITLE $CURR_VER has been detected!!"
    return 0
  fi

  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $Title $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME

  # Now install it!
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make \
  && make install
}
# m4 headers
function inst_m4 {
  URL=$URL_M4
  TITLE="m4"
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ -f "$BREWHOME/bin/m4" ]; then
    echo "$TITLE-$CURR_VER has been detected!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading $Title $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  # Now install it!
  cd $CELLAR/$BLD_DIRNAME && \
  CC="$GCC" CXX="$GXX" \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
  && make \
  && make check \
  && make install
}
# Python2Modules
function inst_python2_addons {
  # rm -rf $BREWHOME/Python2
  set_compiler
  inst_python2
  $PIP2 install -U $PY_MODULES
  inst_pyqt4 $PYTHON2
  # installing numerical modules except numpy
  inst_openblas $PYTHON2
  $PIP2 install -U \
    $SCI_PY_MODULES
}
# Python3Modules
function inst_python3_addons {
  # rm -rf $BREWHOME/Python3
  set_compiler
  inst_python3
  $PIP3 install -U $PY_MODULES
  inst_pyqt4 $PYTHON3
  # installing numerical modules except numpy
  inst_openblas $PYTHON3
  $PIP3 install -U \
    $SCI_PY_MODULES
}
# HDF5
function inst_HDF5 {
  URL=$URL_HDF5
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/lib/libhdf5_*.so.* ; then
    echo "HDF5 library found!! at $BREWHOME/lib64"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading HDF5 $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  LDFLAGS=$RPATH \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --enable-fortran \
    --enable-cxx \
    --enable-hl \
    --enable-java \
  && make -j $PROCESSES \
  && make install
}
# Zenity
function inst_zenity {
  URL=$URL_ZENITY
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/zenity ; then
    echo "$BREWHOME/bin/zenity found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Emacs $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  cd $CELLAR/$DIRNAME && \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    $CELLAR/$DIRNAME/configure --prefix=$BREWHOME && \
    make -j 8 && make install
}
# Emacs
function inst_emacs {
  inst_giflib
  URL=$URL_EMACS
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/emacs ; then
    echo "$BREWHOME/bin/emacs found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Emacs $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  # Bootstrap
  cd $CELLAR/$DIRNAME && \
  $CELLAR/$DIRNAME/autogen.sh && \
  cd $CELLAR
  mkdir $CELLAR/$BLD_DIRNAME
  cd $CELLAR/$BLD_DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  LDFLAGS=$RPATH \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --localstatedir=$BREWHOME/var \
  && make && make install
}
function inst_vim {
  inst_git
  URL=$URL_VIM
  #FILENAME=$(basename "$URL")
  DIRNAME="vim_git"
  #CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/vim ; then
    echo "$BREWHOME/bin/vim found!!"
    return 0
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Checking out vim from github..."
    git clone https://github.com/vim/vim.git $CELLAR/$DIRNAME
  else
    echo "vim Github repo found, updating to current version..."
    cd $CELLAR/$DIRNAME && git pull && cd -
  fi
  cd $CELLAR/$DIRNAME && \
  CC=$GCC CXX=$GXX \
  CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
  LDFLAGS=$RPATH \
  $CELLAR/$DIRNAME/configure \
    --prefix=$BREWHOME \
    --with-features=huge \
    --with-tlib=ncursesw \
  && make && make install
}
function inst_gtksourceview {
  URL=$URL_GTKSOURCEVIEW
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading GTKSourceView $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME && \
    cd $CELLAR/$BLD_DIRNAME && \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    $CELLAR/$DIRNAME/configure --prefix=$BREWHOME && \
    make -j 8 && make install
}
# Mousepad the editor
function inst_mousepad {
  inst_gtksourceview
  URL=$URL_MOUSEPAD
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/mousepad ; then
    echo "$BREWHOME/bin/mousepad found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Mousepad $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME && \
    cd $CELLAR/$BLD_DIRNAME && \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    $CELLAR/$DIRNAME/configure --prefix=$BREWHOME --enable-keyfile-settings && \
    make && make install
}
# install UnRAR
function inst_unrar {
  URL=$URL_UNRAR
  FILENAME=$(basename "$URL")
  DIRNAME="unrar"
  CURR_VER=$(get_version $FILENAME)
  if cmd_exists $BREWHOME/bin/unrar ; then
    echo "$BREWHOME/bin/emacs found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading Emacs $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  cd $CELLAR/$DIRNAME && \
    make -f makefile &&
    install -v -m755 unrar $BREWHOME/bin
}
# install MPICH
function inst_mpich {
  URL=$URL_MPICH
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build
  if cmd_exists $BREWHOME/bin/mpiexec ; then
    echo "$BREWHOME/bin/mpiexec found!!"
    return 0
  fi
  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading MPICH $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME && \
    cd $CELLAR/$BLD_DIRNAME && \
    CC=gcc CXX=g++ FC=gfortran \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    $CELLAR/$DIRNAME/configure \
                --prefix=$BREWHOME \
                --enable-fast=O3,ndebug \
                --disable-error-checking \
                --without-timing \
                --enable-fc \
                --enable-cxx \
                --enable-threads=multiple \
                && make && make install
}
function inst_zeromq {
  URL=$URL_ZEROMQ
  FILENAME=$(basename "$URL")
  DIRNAME="${FILENAME%.*.*}"
  CURR_VER=$(get_version $FILENAME)
  BLD_DIRNAME=$DIRNAME-build

  if [ ! -f $CELLAR/$FILENAME ]; then
    echo "Downloading ZeroMQ $CURR_VER..."
    wget "$URL" -O $CELLAR/$FILENAME
  else
    echo "$CELLAR/$FILENAME found!!"
  fi
  if [ ! -d $CELLAR/$DIRNAME ]; then
    echo "Extracting $CELLAR/$FILENAME"
    tar xf $CELLAR/$FILENAME -C $CELLAR
  fi
  if [ -d $CELLAR/$BLD_DIRNAME ]; then
    echo "Deleting previous compilation directory."
    rm -rf $CELLAR/$BLD_DIRNAME
  fi
  mkdir $CELLAR/$BLD_DIRNAME && \
    cd $CELLAR/$BLD_DIRNAME && \
    CFLAGS="$GCC_OPTS" CXXFLAGS="$GXX_OPTS" \
    $CELLAR/$DIRNAME/configure --prefix=$BREWHOME && \
    make -j 8 && make install
}
# install all
function inst_all {
  inst_zenity
  inst_compiler
  inst_autoconf
  inst_automake
  inst_m4
  inst_mpich
  inst_perl
  inst_git
  inst_lxterminal
  inst_zeromq
  inst_sqlite3
  inst_python2
  inst_qt4
  inst_cmake
  inst_python3_modules
  inst_python2_modules
  inst_ROOT6
  inst_Geant4
  inst_boost
  inst_emacs
  inst_vim
  inst_mousepad
  inst_unrar
}
# Exit message
function exit_message {
  echo "Consider putting $BREWHOME/bin into your PATH, $BREWHOME/lib/../lib64 into your LD_LIBRARY_PATH, and $BREWHOME/include to C_INCLUDE_DIR."
}
# Show status
function show_status {
  echo ""
  echo "Current compiler: $COMPILER"
  echo "Current Installation Prefix: $BREWHOME"
  echo "Current Wine Cellar Path: $CELLAR"
  echo ""
}
# Checking directory exsistence.
function check_dir {
  if [ ! -d "$BREWHOME" ] ; then
    echo "Generating $BREWHOME directory."
    mkdir $BREWHOME
  else
    echo "Looks like $BREWHOME directory exists already!!"
  fi
  if [ ! -d "$CELLAR" ] ; then
    echo "Generating $CELLAR directory."
    mkdir $CELLAR
  else
    echo "Looks like $CELLAR directory exists already!!"
  fi
}
OPTIONS="Switch_Compiler Change_Prefix \
     ALL gcc gcc_cuda LLVM(clang) yasm \
     gdb Boost GLib MPICH \
     Qt4 Qt_Webkit Phonon DConf \
     cmake Python2 Python3 Python2_addons Python3_addons \
     Lua Perl \
     Gnuplot \
     ROOT6 Geant4 HDF5 Git LxTerminal Zenity ZeroMQ \
     Autotools \
     Valgrind Valkyrie \
     Emacs Vim Mousepad \
     UnRAR \
     Show_Settings \
     Clean Purge Quit"
GEANT4DATA_SETTINGS=$(cat << "EOF"
export LIBRARY_PATH=$BREWHOME/Geant4/lib64/:$LIBRARY_PATH
export GEANT4_DATA_DIR=$BREWHOME/Geant4/share/Geant4-10.2.0/data
export G4LEDATA=$GEANT4_DATA_DIR/G4EMLOW6.48
export G4LEVELGAMMADATA=$GEANT4_DATA_DIR/PhotonEvaporation3.1
export G4NEUTRONHPDATA=$GEANT4_DATA_DIR/G4NDL4.5
export G4NEUTRONXSDATA=$GEANT4_DATA_DIR/G4NEUTRONXS1.4
export G4PIIDATA=$GEANT4_DATA_DIR/G4PII1.3
export G4RADIOACTIVEDATA=$GEANT4_DATA_DIR/RadioactiveDecay4.2
export G4REALSURFACEDATA=$GEANT4_DATA_DIR/RealSurface1.0
export G4SAIDXSDATA=$GEANT4_DATA_DIR/G4SAIDDATA1.1
export G4ABLADATA=$GEANT4_DATA_DIR/G4ABLA3.0
export G4ENSDFSTATEDATA=$GEANT4_DATA_DIR/G4ENSDFSTATE1.2
EOF
)
ROOT6_SETTINGS=$(cat << "EOF"
export ROOTSYS=$BREWHOME/root
source $BREWHOME/root/bin/thisroot.sh
EOF
)
# The main function
function main {
  export LDFLAGS=$RPATH
  show_status
  check_dir
  echo ""
  get_py_ver
  echo ""
  select opt in $OPTIONS; do
    if [ "$opt" = "Quit" ] ; then
      echo "Exiting"
      exit
    elif [ "$opt" = "LLVM(clang)" ] ; then
      check_dir
      inst_llvm_clang
      exit
    elif [ "$opt" = "Switch_Compiler" ] ; then
      if [[ $COMPILER == "gcc" ]] ; then
        set_compiler "llvm"
      elif [[ $COMPILER == "llvm" ]] ; then
        set_compiler "gcc"
      fi
      show_status
    elif [ "$opt" = "Clean" ] ; then
      echo "Cleaning up Cellar"
      rm -rvf $CELLAR/*
      exit
    elif [ "$opt" = "Purge" ] ; then
      echo "****** Ordo malleus exterminatus!! ******"
      rm -rvf $CELLAR $BREWHOME
      exit
    elif [ "$opt" = "gcc" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_gcc
      exit

    elif [ "$opt" = "gcc_cuda" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_gcc_cuda
      exit
    elif [ "$opt" = "Boost" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_boost
      exit
    elif [ "$opt" = "MPICH" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_mpich
      exit
    elif [ "$opt" = "GLib" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_GLib
      exit
    elif [ "$opt" = "gdb" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_gdb
      exit
    elif [ "$opt" = "yasm" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_yasm
      exit
    elif [ "$opt" = "Qt4" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_sqlite3
      inst_qt4
      exit
    elif [ "$opt" = "Qt_Webkit" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_qtwebkit
      exit
    elif [ "$opt" = "DConf" ] ; then
      check_dir
      set_compiler $COMPILER
      inst_dconf_n_dconfedit
      exit
    elif [ "$opt" = "Phonon" ] ; then
      check_dir
      set_compiler $COMPILER
      exit
    elif [ "$opt" = "cmake" ] ; then
      set_compiler $COMPILER
      inst_cmake
      exit_message
      exit
    elif [ "$opt" = "Python2" ]; then
      set_compiler $COMPILER
      inst_python2
      exit_message
      exit
    elif [ "$opt" = "Python2_addons" ]; then
      set_compiler $COMPILER
      inst_python2_addons
      exit_message
      exit
    elif [ "$opt" = "Python3" ] ; then
      set_compiler $COMPILER
      inst_python3
      exit_message
      exit
    elif [ "$opt" = "Python3_addons" ]; then
      set_compiler $COMPILER
      inst_python3_addons
      exit_message
      exit
    elif [ "$opt" = "Lua" ]; then
      set_compiler $COMPILER
      inst_lua
      exit_message
      exit

    elif [ "$opt" = "Perl" ]; then
      set_compiler $COMPILER
      inst_perl
      exit_message
      exit
    elif [ "$opt" = "Gnuplot" ]; then
      set_compiler $COMPILER
      inst_gnuplot
      exit_message
      exit

    elif [ "$opt" = "ROOT6" ] ; then
      set_compiler $COMPILER
      inst_ROOT6
      exit_message
      exit
    elif [ "$opt" = "Geant4" ] ; then
      set_compiler $COMPILER
      inst_Geant4
      exit_message
      exit
    elif [ "$opt" = "HDF5" ] ; then
      set_compiler $COMPILER
      inst_HDF5
      exit_message
      exit
    elif [ "$opt" = "LxTerminal" ] ; then
      set_compiler $COMPILER
      inst_lxterminal
      exit_message
      exit
    elif [ "$opt" = "Git" ] ; then
      set_compiler $COMPILER
      inst_git
      exit_message
      exit
    elif [ "$opt" = "Autotools" ] ; then
      set_compiler $COMPILER
      inst_autoconf
      inst_automake
      inst_m4
      exit_message
      exit
    elif [ "$opt" = "Valgrind" ] ; then
      set_compiler $COMPILER
      inst_valgrind
      exit
    elif [ "$opt" = "Valkyrie" ] ; then
      set_compiler $COMPILER
      inst_valkyrie
      exit
    elif [ "$opt" = "Emacs" ] ; then
      set_compiler $COMPILER
      inst_emacs
      exit
    elif [ "$opt" = "Vim" ] ; then
      set_compiler $COMPILER
      inst_vim
      exit
    elif [ "$opt" = "Mousepad" ] ; then
      set_compiler $COMPILER
      inst_mousepad
      exit
    elif [ "$opt" = "UnRAR" ] ; then
      set_compiler $COMPILER
      inst_unrar
      exit
    elif [ "$opt" = "Zenity" ] ; then
      set_compiler $COMPILER
      inst_zenity
      exit

    elif [ "$opt" = "ZeroMQ" ] ; then
      set_compiler $COMPILER
      inst_zeromq
      exit
    elif [ "$opt" = "Change_Prefix" ] ; then
      echo "Type new prefix (example: /home/johndoe/homebrew/)"
      read NEW_PREFIX
      if [ -z "$NEW_PREFIX" ] ; then
        echo "Empty input!! reverting to default!!"
        BREWHOME=$HOME/local
      else
        BREWHOME=$NEW_PREFIX
      fi
      CELLAR=$BREWHOME/cellar
      check_dir
      show_status
    elif [ "$opt" = "Show_Settings" ] ; then
      echo "********* Geant4 Settings *********"
      echo "$GEANT4DATA_SETTINGS"
      echo ""
      echo "********* ROOT6 Settings *********"
      echo "$ROOT6_SETTINGS"
      echo ""
    elif [ "$opt" = "ALL" ]; then
      if [[ $COMPILER == "llvm" ]] ; then
        set_compiler "gcc"
        inst_gcc
        inst_python2
        inst_llvm_clang
        set_compiler "llvm"
      else
        inst_compiler
      fi
      inst_all
      exit_message
      exit
    else
      echo "Oops, bad option!!"
    fi
  done
}
# Run the script!!
main
