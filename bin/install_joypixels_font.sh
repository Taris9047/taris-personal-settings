#!/usr/bin/env bash
#
# Let's install JoyPixels fonts!
#

font_url="https://archlinux.org/packages/community/any/ttf-joypixels/download"
SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
tmp_dir="/tmp/.inst_joypixels_tmpdir/"
CWD="$(pwd -P)"

font_dest_dir="${HOME}/.local/share/fonts/JoyPixels/"

echo 'Installing JoyPixels!! (from Network)'
if [ ! -d "${tmp_dir}" ]; then
    mkdir -pv "${tmp_dir}" > /dev/null 2>&1 
fi
if [ ! -d "$font_dest_dir" ]; then
    mkdir -pv "${font_dest_dir}" > /dev/null 2>&1
fi

if [ ! -x "$(command -v unzstd)" ]; then
    printf 'Ooops, we need zstd tools...!!\n'
    if [ -n "$(grep -i 'rhel' /etc/os-release)" ]; then
        [ -x "$(command -v dnf)" ] && sudo dnf install -y zstd || sudo yum install -y zstd
    fi
fi

cd "${tmp_dir}"
wget "${font_url}" -O /tmp/ttf-joypixels-6.6.0-1-any.pkg.tar.zst > /dev/null 2>&1
tar --use-compress-program=unzstd -xvf /tmp/ttf-joypixels-6.6.0-1-any.pkg.tar.zst -C "${tmp_dir}"
cp -vfr "${tmp_dir}/usr/share/fonts/joypixels/*.ttf ${font_dest_dir}/" > /dev/null 2>&1 

cd "${CWD}" 

echo "Cleaning up temp. dirs!!"
rm -rf "${tmp_dir}"

echo "Updating Fonts Cache"
fc-cache -fv > /dev/null 2>&1

echo "Joypixels fonts Installed!!"
