#!/bin/bash

# Installs nanum fonts to local directory.

if [ ! -x "$(command -v unzip)" ]; then
    echo "Uh oh.. we need unzip!! Install it to use this script!"
    exit -1
fi

if [ ! -x "$(command -v wget)" ]; then
    echo "Uh oh.. we need wget!! Install it to use this script!"
    exit -1
fi

CWD=$(pwd -P)
HOMEBREW="${HOME}/.local"
TARGET_DIR="${HOMEBREW}/share/fonts/NanumFonts"

NGF_URL="http://cdn.naver.com/naver/NanumFont/fontfiles/NanumFont_TTF_ALL.zip"
NGFC_URL="https://github.com/naver/nanumfont/releases/download/VER2.5/NanumGothicCoding-2.5.zip"

TEMP_DIR="${CWD}/.temp_nanumfonts"
if [ ! -d "${TEMP_DIR}" ]; then
    mkdir -p "${TEMP_DIR}"
fi

wget "$NGF_URL" -O "$TEMP_DIR/ngf.zip"
wget "$NGFC_URL" -O "$TEMP_DIR/ngfc.zip"

if [ ! -d "${TARGET_DIR}" ]; then
    mkdir -p "${TARGET_DIR}"
fi

unzip "${TEMP_DIR}/ngf.zip" -do "${TARGET_DIR}/"
unzip "${TEMP_DIR}/ngfc.zip" -do "${TARGET_DIR}/"

rm -rf "${TARGET_DIR}/__MACOSX"

fc-cache -fv

rm -rf "$TEMP_DIR"

echo ""
echo "Jobs finished!!"
echo ""
