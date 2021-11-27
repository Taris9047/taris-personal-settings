#!/usr/bin/env bash
#
# Let's install FiraCode to make everything Prettier in Emacs!
#

firacode_url="https://github.com/tonsky/FiraCode/releases/download/5.2/Fira_Code_v5.2.zip"
SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
tmp_dir="/tmp/.inst_firacode_tmpdir/"
CWD="$(pwd -P)"

font_dest_dir="${HOME}/.local/share/fonts/FiraCode/"

echo 'Installing FireCode!! (from Network)'
if [ ! -d "${tmp_dir}" ]; then
    mkdir -pv "${tmp_dir}" > /dev/null 2>&1 
fi
if [ ! -d "$font_dest_dir" ]; then
    mkdir -pv "${font_dest_dir}" > /dev/null 2>&1
fi

cd "${tmp_dir}"
wget "${firacode_url}" > /dev/null 2>&1
unzip "./Fira_Code_v5.2.zip" > /dev/null 2>&1
cp -vfr ./ttf/*.ttf "${font_dest_dir}/" > /dev/null 2>&1 

cd "${CWD}" 

echo "Cleaning up temp. dirs!!"
rm -rf "${tmp_dir}"

echo "Updating Fonts Cache"
fc-cache -fv > /dev/null 2>&1

echo "FiraCode fonts Installed!!"
