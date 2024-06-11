#!/bin/bash
########
#
#  adopted from:
#  https://gist.githubusercontent.com/kenny-kvibe/b1b7c4948c7406309eb66da3ed2a8144/raw/aad39cff25da8f0492447ac1d9a48a866d5a3680/figlet_install_fonts.sh
#
#  Install "figlet" package
#  & Download figlet fonts
#    (running as root)
#
# + Github Repositories:
#--> https://github.com/xero/figlet-fonts
#--> https://github.com/phracker/figlet-fonts
#--> https://github.com/cmatsuoka/figlet
#
# + Websites:
#--> http://www.figlet.org/
#--> http://www.jave.de/
#
########
#
#  Globally Used Variables
#
separator='========================'
figdir=/usr/share/figlet
nul=/dev/null
tmpd=
chk=
########
#
#  Check if running as root
#
test $UID -eq 0 && test `/usr/bin/id -u` -eq 0
[[ $? -ne 0 ]] && echo 'Insufficient permissions, please run as root to install properly, exiting...' \
               && exit 1
########
#
#  Check for Script-Required Dependencies:
#    apt, awk, curl, git, stat, sudo, wc
#
_deps=(apt awk curl git stat sudo wc)
for chk in ${_deps[@]}; do
	test ! -x "$(which $chk)" && printf "Missing required dependency: \"$1\"\nExiting...\n" \
	    && exit 1
done
unset _deps
########
#
#  Remove Duplicates - Function
#
function rem_duplicates {
	local files=(`ls *.fl{f,c}`)
	local file1 file2
	local sizes last range i j
	let last=${#files[*]}-1
	range=`eval echo "{0..$last}"`
	unset last
	for i in $range; do
		file1=${files[$i]}
		[[ ! -f "$file1" ]] && continue
		sizes[$i]=`stat -c'%s' "$file1"`;
	done
	for i in $range; do
		file1="${files[$i]}"
		[[ ! -f $file1 ]] && continue
		for j in $range; do
			file2="${files[$j]}"
			[[ "$file1" = "$file2" || ! -f "$file1" || ! -f "$file2" ]] && continue
			if [[ ${sizes[$i]} -eq ${sizes[$j]} && "${file1,,}" = "${file2,,}" ]]; then
				echo "Found duplicate of '$file2'. Deleting '$file1' file..."
				/bin/rm -rf "$file1" &>$nul
				[[ -f $file1 ]] && echo "File '$file1' was not deleted." \
				                || echo "File '$file1' deleted sucessfully!"
			fi
		done
	done
	return 0
}
########
#
#  Git Download - Function
#
function git_download {
	local _url="$1"  # github repo url
	[[ -z "$_url" ]] && return 1
	local _fdir="`echo $_url | cut -d'/' -f5`"
	[[ ! -z "$2" ]] && _fdir="${_fdir}/${2}"  # fonts dir path repo relative
	echo $separator
	echo "Downloading figlet fonts: \"${_url}/\"..."
	git clone "$_url"
	( cd "$_fdir"; /bin/mv -f *.fl{f,c} $tmpd &>$nul; exit 0; )
	/bin/rm -rf "$_fdir" &>$nul
	return 0
}
########
#
#  WWW Download - Function
#
function www_download {
	local _url="$1"   # target url
	[[ -z "$_url" ]] && return 1
	local _fdir="$2"  # target font dir name
	local _fpth="$3"  # target font-list file path
	local _fprm="$4"  # target download fonts url param
	local _qnum="$5"  # target html quote seperator field number
	local _hsep="$6"  # target html attribute value seperator
	local _hnum="$7"  # target html attribute value seperator number
	echo $separator
	echo "Downloading figlet fonts: \"${_url}/\"..."
	mkdir "$_fdir"
	curl -s "${_url}/${_fpth}" \
	    | grep -e '\.flf' -e '\.flc' \
	    | cut -d'"' -f"${_qnum}" \
	    | awk  -F "${_hsep}" "{ print \"curl -s -o '${_fdir}/\"\$${_hnum}\"' '${_url}/${_fprm}\"\$${_hnum}\"'\" }" \
	    | sh
	( cd "${_fdir}"; /bin/mv -f *.fl{f,c} $tmpd &>$nul; exit 0; )
	/bin/rm -rf "$_fdir" &>$nul
	return 0
}
########
#
#  Figlet Main - Function
#
function figlet_main {
	#
	#  Install "figlet" Package & its Dependencies
	#  Skip if already installed
	#
	local _install_figlet _pkgs=(libc6 figlet)
	function _install_figlet {
		chk= apt update &>$nul
		for chk in ${_pkgs[@]}; do
			apt install -y $chk --reinstall &>$nul
			[[ $? -ne 0 ]] && printf "\nThere was an error processing package: ${chk}\nTry running manually: \`apt install ${chk}\`\n\n" \
			    && exit 1
		done
	}
	test ! -x "$(which figlet)" && _install_figlet
	printf "\n$separator\n"
	IFS=, && echo "Done processing packages: ${_pkgs[*]}" | sed "s/$IFS/$IFS /g" && unset IFS
	unset _install_figlet _pkgs
	#
	#  Create a Temporary Directory
	#  & Move into it
	#
	local _tmpd="$(/bin/mktemp -d)"
	tmpd="${_tmpd}/.fonts"
	cd "$_tmpd"
	echo $separator
	echo "Working directory \"$_tmpd\""
	mkdir $tmpd
	#
	#  Git Download figlet fonts
	#
	git_download 'https://github.com/xero/figlet-fonts'     ''
	git_download 'https://github.com/phracker/figlet-fonts' ''
	git_download 'https://github.com/cmatsuoka/figlet'      'fonts'
  git_download 'https://github.com/xero/figlet-fonts.git' ''
	#
	#  WWW Download figlet fonts
	#
	www_download 'http://www.figlet.org' 'figlet-fontdb' 'fontdb.cgi'                 'fontdb_example.cgi?font=' '2' '=' '2'
	www_download 'http://www.jave.de'    'figlet-jave'   'figlet/fonts/overview.html' 'figlet/fonts/details/'    '6' '/' '3'
	#
	#  Save the Fonts Categories Tree file
	#    from the "phracker/figlet-fonts" github repository
	#    same file exists on "www.jave.de" inside "figletfonts40.zip"
	#
	curl -s 'https://raw.githubusercontent.com/phracker/figlet-fonts/master/categoriestree.txt' \
	     -o "${tmpd}/categoriestree.txt"
	#
	#  Remove Duplicate Fonts
	#  & Move all font-files to figlet-fonts Directory
	#  & Set the font-files Permissions
	#
	cd $tmpd
	echo $separator
	echo "Working directory \"${tmpd}\""
	rem_duplicates
	/bin/mv -f ./*.{flf,flc,txt} $figdir &>$nul
	chown root:root $figdir/*.{flf,flc,txt} &>$nul
	chmod 644 $figdir/*.{flf,flc,txt} &>$nul
	#
	#  Remove the Temporary Directory
	#
	echo $separator
	cd ../..
	/bin/rm -rf "$_tmpd" &>$nul
	#####
	echo "Moved figlet fonts to \"$figdir\" & Deleted the temporary download directories."
	echo "Currently installed `ls -ld $figdir/*.fl{f,c} | wc -l` fonts."
	echo 'Done.'
	return 0
}
########
#
#  Run & Exit
#
printf 'Installing "figlet" and figlet fonts\n'
printf "Will overwrite the fonts (if any) in the \"$figdir\" directory\n"
printf 'Do you want to continue ? [Y/n]: '; chk= && read -r -N1 chk
[[ "$chk" = 'y' || "$chk" = 'Y' ]] && figlet_main
unset figlet_main www_download git_download separator figdir chk tmpd nul
printf '\nExiting...\n'
exit 0
