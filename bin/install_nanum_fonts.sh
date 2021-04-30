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

CWD="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
HOMEBREW="${HOME}/.local"
TARGET_DIR="${HOMEBREW}/share/fonts/NanumFonts"

NF_URL="https://github.com/Taris9047/taris-personal-settings/releases/download/Nanum/NanumFonts.zip"
# NGF_URL="http://cdn.naver.com/naver/NanumFont/fontfiles/NanumFont_TTF_ALL.zip"
# NGFC_URL="https://github.com/naver/nanumfont/releases/download/VER2.5/NanumGothicCoding-2.5.zip"

TEMP_DIR="${CWD}/.temp_nanumfonts"
if [ ! -d "${TEMP_DIR}" ]; then
    mkdir -p "${TEMP_DIR}"
fi

# wget "$NGF_URL" -O "$TEMP_DIR/ngf.zip"
# wget "$NGFC_URL" -O "$TEMP_DIR/ngfc.zip"
wget "$NF_URL" -O "$TEMP_DIR/nf.zip"

if [ ! -d "${TARGET_DIR}" ]; then
    mkdir -p "${TARGET_DIR}"
fi

# unzip "${TEMP_DIR}/ngf.zip" -do "${TARGET_DIR}/"
# unzip "${TEMP_DIR}/ngfc.zip" -do "${TARGET_DIR}/"
# cd "${TARGET_DIR}" && unzip "${TEMP_DIR}/ngf.zip"
# cd "${TARGET_DIR}" && unzip "${TEMP_DIR}/ngfc.zip"
cd "${TARGET_DIR}" && unzip "${TEMP_DIR}/nf.zip"

# rm -rf "${TARGET_DIR}/__MACOSX"

fc-cache -fv > /dev/null 2>&1

rm -rf "$TEMP_DIR"

echo ""
echo "Nanum Font Installer - Jobs finished!!"
echo ""
