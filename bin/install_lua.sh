#!/bin/sh

#
# Lua 5.4 install script.
#

URL='https://www.lua.org/ftp/lua-5.4.6.tar.gz'
PATCH_URL='https://www.linuxfromscratch.org/patches/blfs/12.1/lua-5.4.6-shared_library-1.patch'

WORK_DIR="#(mktemp -d -t lua5.4-XXXXXXXXXXXXXXXX)"
CURR_DIR=`pwd`

cd "${WORK_DIR}"

wget "${URL}" -O ./lua-5.4.6.tar.gz
wget "${PATCH_URL}"
tar xvf ./lua-5.4.6.tar.gz

cd ./lua-5.4.6
patch -Npl -i ../lua-5.4.6-shared_library-1.patch && make linux

# Making pkgconfig file
cat > lua.pc << "EOF"
V=5.4
R=5.4.6

prefix=/usr/local
INSTALL_BIN=${prefix}/bin
INSTALL_INC=${prefix}/include
INSTALL_LIB=${prefix}/lib
INSTALL_MAN=${prefix}/share/man/man1
INSTALL_LMOD=${prefix}/share/lua/${V}
INSTALL_CMOD=${prefix}/lib/lua/${V}
exec_prefix=${prefix}
libdir=${exec_prefix}/lib
includedir=${prefix}/include

Name: Lua
Description: An Extensible Extension Language
Version: ${R}
Requires:
Libs: -L${libdir} -llua -lm -ldl
Cflags: -I${includedir}
EOF



sudo make INSTALL_TOP=/usr/local \
          INSTALL_DATA="cp -d" \
          INSTALL_MAN=/usr/local/share/man/man1 \
          TO_LIB="liblua.so liblua.so.5.4 liblua.so.5.4.6" \
          install && \
sudo mkdir -pv                             /usr/local/share/doc/lua-5.4.6 && \
sudo cp -v doc/*.{html,css,gif,png}        /usr/local/share/doc/lua-5.4.6 && \
install -v -m644 -D lua.pc                 /usr/local/lib/pkgconfig/lua.pc

printf 'Lua-5.4.2 is now installed at /usr/local\n'
