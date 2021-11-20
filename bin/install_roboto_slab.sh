#!/usr/bin/env bash
#
# Let's install FiraCode to make everything Prettier in Emacs!
#

font_path="https://github.com/googlefonts/robotoslab.git"
SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
tmp_dir="$SCRIPTPATH/.inst_robotoslab_tmpdir/"
CWD="$(pwd -P)"

font_dest_dir="${HOME}/.local/share/fonts/RobotoSlab/"

if [ ! -x "$(command -v git)" ]; then
    printf 'We need git for this crap!\n'
    exit 1
fi

echo 'Installing Roboto Slab!! (from Network)'
if [ ! -d "${tmp_dir}" ]; then
    mkdir -pv "${tmp_dir}" > /dev/null 2>&1 
fi
if [ ! -d "$font_dest_dir" ]; then
    mkdir -pv "${font_dest_dir}" > /dev/null 2>&1
fi

cd "${tmp_dir}" || exit
git clone "${font_path}" "./robotoslab"
cp -vfr ./robotoslab/fonts/ttf/*.ttf "${font_dest_dir}/" > /dev/null 2>&1 
cd "${CWD}" 

echo "Cleaning up temp. dirs!!"
rm -rf "${tmp_dir}"

echo "Updating Fonts Cache"
fc-cache -fv > /dev/null 2>&1

echo "Roboto Slab fonts Installed!!"
