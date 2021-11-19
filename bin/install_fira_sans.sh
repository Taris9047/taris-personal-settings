#!/usr/bin/env bash
#
# Let's install FiraCode to make everything Prettier in Emacs!
#

font_path="https://github.com/bBoxType/FiraSans.git"
SCRIPTPATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
tmp_dir="$SCRIPTPATH/.inst_firasans_tmpdir/"
CWD="$(pwd -P)"

font_dest_dir="${HOME}/.local/share/fonts/FiraSans/"

if [ ! -x "$(command -v git)" ]; then
    printf 'We need git for this crap!\n'
    exit 1
fi

echo 'Installing FireSans!! (from Network)'
if [ ! -d "${tmp_dir}" ]; then
    mkdir -pv "${tmp_dir}" > /dev/null 2>&1 
fi
if [ ! -d "$font_dest_dir" ]; then
    mkdir -pv "${font_dest_dir}" > /dev/null 2>&1
fi

cd "${tmp_dir}" || exit
git clone "${font_path}"
cp -vfr ./FiraSans/Fira_Sans_4_3/Fonts/Fira_Sans_TTF_4301/Normal/Roman/*.ttf "${font_dest_dir}/" > /dev/null 2>&1 
cp -vfr ./FiraSans/Fira_Sans_4_3/Fonts/Fira_Sans_TTF_4301/Normal/Italic/*.ttf "${font_dest_dir}/" > /dev/null 2>&1 
cd "${CWD}" 

echo "Cleaning up temp. dirs!!"
rm -rf "${tmp_dir}"

echo "Updating Fonts Cache"
fc-cache -fv > /dev/null 2>&1

echo "FiraSans fonts Installed!!"
