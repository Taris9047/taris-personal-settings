#!/bin/sh
set -e

# My settings Initial installation script
printf '**** Setting up Taris'' Environments ****'

USR_DIR="$HOME"
SETTINGS_DIR="$HOME/.settings"
HOMEBREW="$HOME/.local"
MY_GIT_REPO='git@github.com:Taris9047/taris-personal-settings.git'
MY_UDS_GIT_REPO='git@github.com:Taris9047/uds.git'

printf 'Target directory: %s\n' "${USR_DIR}"
printf 'Source directory: %s\n' "${SETTINGS_DIR}"

# Check git.
[ ! -x "$(command -v git)" ] && printf 'ERROR! No git found!!\n' && exit 1

# Clone settings dir to settings!
[ ! -d "$SETTINGS_DIR" ] && git clone "$MY_GIT_REPO" "$SETTINGS_DIR"
{ [ -d "$SETTINGS_DIR" ] && [ ! -L "$HOME/Settings" ]; } && { ln -sfv "$SETTINGS_DIR" "$HOME/Settings" || true; }

# Make homebrew dir
[ ! -d "$HOMEBREW" ] && mkdir -pv "${HOMEBREW}"
{ [ -d "$HOMEBREW" ] && [ ! -L "$HOME/Homebrew" ]; } && { ln -sfv "$HOMEBREW" "$HOME/Homebrew" || true; }

# OS Detection
lowercase() {
	printf '%s' "$1" | tr '[:upper:]' '[:lower:]'
}

PLATFORM='none'
set_os_type() {
	UNAMESTR=$(lowercase "$(uname)")

	case "$UNAMESTR" in
	*"cygwin"*) PLATFORM="cygwin" ;;
	*"linux"*) PLATFORM="linux" ;;
	*"darwin"*) PLATFORM="darwin" ;;
	*) PLATFORM="general_unix" ;;
	esac

	printf 'Setting platform as: %s\n' "$PLATFORM"
}

# Set OS Type
set_os_type

# Implementing a poor man's array in POSIX way.
# referenced: https://github.com/krebs/array/blob/master/array
#
# Array constructor
array() {
	for i in "$@"; do
		printf '%s\n' "$i" | array_element_encode
	done
}

# Array element eocode/decode
array_element_encode() {
	sed 's/%/%25/g' | sed -e :a -e '$!N; s/\n/%0A/; ta'
}

array_element_decode() {
	sed -e 's/%0[aA]/\
/g' -e 's/%25/%/g'
}

# Config Files - This part needs to be POSIXified!
# CONF_LIST=("vimrc" "gitignore" "gitconfig" "gdbinit" "Xresources" "tmux.conf")
CONF_LIST=$(array 'vimrc' 'gitignore' 'gitconfig' 'gdbinit' 'Xresources' 'tmux.conf' 'profile')

# Linking dotfiles
printf "\nLinking dotfiles from settings dir ...\n"
# Iterate with CONF_LIST
#
# Do this stuff for each element
do_inst() {
	printf 'Installing: %s\n' ".${1}"
	rm -rf "${USR_DIR}/.${1}"
	ln -sf "${SETTINGS_DIR}/dotfiles/${1}" "${USR_DIR}/.${1}" || true
}
# Iterate through the array...
printf '%s\n' "$CONF_LIST" |
	while IFS= read -r element; do
		do_inst "$(printf '%s\n' "$element" | array_element_decode)"
	done
# Lastly, prepare the gitconfig.local
[ -L "$HOME/.gitconfig" ] && [ ! -f "$HOME/.gitconfig.local" ] && touch "$HOME/.gitconfig.local"

# Still linking dotfiles... but to some odd locations.
# i.e. .config or .local/config directories etc.
printf '\nWorking on other config files ...\n'
# VIM
printf 'Setting up VIM package directory\n'
VIM_CONF_DIR="$USR_DIR/.vim"
VIM_SETTINGS_DIR="$SETTINGS_DIR/dotfiles/vim"
[ ! -d "$VIM_CONF_DIR" ] && rm -rf "$VIM_CONF_DIR"
mkdir -pv "$VIM_CONF_DIR"
# ln -sf "$VIM_SETTINGS_DIR/autoload" "$VIM_CONF_DIR/autoload"
ln -sf "$VIM_SETTINGS_DIR/colors" "$VIM_CONF_DIR/colors"
mkdir -p "$VIM_CONF_DIR/pack"
mkdir -p "$VIM_CONF_DIR/terminal_colors"

# NVIM
printf 'Setting up NVIM config file\n'
NVIM_CONF_HOME="$USR_DIR/.config/nvim"
NVIM_GTK_CONF_HOME="$USR_DIR/.config/nvim-gtk"
[ ! -d "$NVIM_CONF_HOME" ] && mkdir -pv "$NVIM_CONF_HOME"
[ ! -d "$NVIM_GTK_CONF_HOME" ] && mkdir -pv "$NVIM_GTK_CONF_HOME"
rm -rf "$NVIM_CONF_HOME/*init.vim"
ln -sf "$SETTINGS_DIR/dotfiles/init.vim.nvim" "$NVIM_CONF_HOME/init.vim" || true
ln -sf "$SETTINGS_DIR/dotfiles/init.vim.nvim" "$NVIM_CONF_HOME/sysinit.vim" || true
ln -sf "$SETTINGS_DIR/dotfiles/ginit.vim.nvim" "$NVIM_CONF_HOME/ginit.vim" || true

# Micro Editor
printf 'Setting up micro config files\n'
MICRO_CONF_HOME="$USR_DIR/.config/micro"
[ ! -d "$MICRO_CONF_HOME" ] && mkdir -pv "$MICRO_CONF_HOME"
rm -rf "${MICRO_CONF_HOME:?}/*"
ln -sf "$SETTINGS_DIR/dotfiles/micro/bindings.json" "$MICRO_CONF_HOME/bindings.json"
ln -sf "$SETTINGS_DIR/dotfiles/micro/settings.json" "$MICRO_CONF_HOME/settings.json"
ln -sf "$SETTINGS_DIR/dotfiles/micro/init.lua" "$MICRO_CONF_HOME/init.lua"

# Fish
printf 'Setting up fish config file\n'
FISH_CONF_HOME="$USR_DIR/.config/fish"
[ ! -d "$FISH_CONF_HOME" ] && mkdir -pv "$FISH_CONF_HOME"
rm -rf "${FISH_CONF_HOME:?}/*"
ln -sf "$SETTINGS_DIR/dotfiles/my_settings_fish" "$FISH_CONF_HOME/config.fish" || true

# Fontconfig
printf 'Setting up Fontconfig dir\n'
FONTCONFIG_DIR="$HOME/.config/fontconfig"
[ ! -d "$FONTCONFIG_DIR" ] && mkdir -pv "$FONTCONFIG_DIR"
rm -rf "${FONTCONFIG_DIR}/fonts.conf"
ln -sf "${SETTINGS_DIR}/dotfiles/fonts.conf" "${FONTCONFIG_DIR}/fonts.conf" || true

# Starship
printf 'Setting up starship config file\n'
STARSHIP_CONF_FILE="$USR_DIR/.config/starship.toml"
[ ! -f "$STARSHIP_CONF_FILE" ] && (ln -sfv "$SETTINGS_DIR/dotfiles/starship.toml" "$STARSHIP_CONF_FILE" || true)

# Alacritty - an OpenGL based terminal
printf 'Setting up Alacritty config file\n'
ALACRITTY_CONF_FILE="$USR_DIR/.config/alacritty.yml"
rm -rf "$ALACRITTY_CONF_FILE"
ln -sf "$SETTINGS_DIR/dotfiles/alacritty.yml" "$ALACRITTY_CONF_FILE" || true

# Kitty - A highly customizable terminal
printf 'Setting up Kitty config file\n'
KITTY_CONF_FILE="$USR_DIR/.config/kitty/kitty.conf"
rm -rf "$KITTY_CONF_FILE"
rm -rf "$USR_DIR/.config/kitty"
mkdir -p "$USR_DIR/.config/kitty"
ln -sf "$SETTINGS_DIR/dotfiles/kitty.conf" "$KITTY_CONF_FILE" || true

# Emacs - Not Doom nor Space. Just Emacs.
if [ ! -d "$HOME/.doom.d" ]; then
	printf 'Setting up Default emacsd.d\n'
	EMACSD="$HOME/.emacs.d"
	[ ! -d "$EMACSD" ] && mkdir -pv "$EMACSD"
	rm -rf "${EMACSD:?}/*"
	ln -sf "$SETTINGS_DIR/dotfiles/emacs.d/init.el" "$EMACSD/init.el" || true
	ln -sf "$SETTINGS_DIR/dotfiles/emacs.d/config.org" "$EMACSD/config.org" || true
fi

# Tmux!
printf 'Setting up Tmux Package Manager (TPM)\n'
[ ! -d "$HOME/.tmux/plugins/tpm" ] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Awesome desktop - Just for my Arcolinux
ASMWM_CNFDIR="$HOME/.config/awesome"
if [ -d "$ASMWM_CNFDIR" ]; then
	printf 'Setting up Awesome desktop - Why hate lua?\n'
	mv -f "$ASMWM_CNFDIR/autostart.sh" "$ASMWM_CNFDIR/autostart.sh.bak"
	ln -sf "$SETTINGS_DIR/dotfiles/awesome/autostart.sh" "$ASMWM_CNFDIR/autostart.sh"
	mv -f "$ASMWM_CNFDIR/system-overview" "$ASMWM_CNFDIR/system-overview.bak"
	ln -sf "$SETTINGS_DIR/dotfiles/awesome/system-overview" "$ASMWM_CNFDIR/system-overview"
	mv -f "$ASMWM_CNFDIR/rc.lua" "$ASMWM_CNFDIR/rc.lua.bak"
	ln -sf "$SETTINGS_DIR/dotfiles/awesome/rc.lua" "$ASMWM_CNFDIR/rc.lua"
fi

# Setting up RClone directories.
printf 'Setting up RClone directories\n'
GOOGLE_DRIVE="$HOME/.google-drive"
GOOGLE_DRIVE_SYM="$HOME/GoogleDrive"
ONE_DRIVE="$HOME/.onedrive"
ONE_DRIVE_SYM="$HOME/OneDrive"
[ ! -d "$GOOGLE_DRIVE" ] && mkdir -pv "$GOOGLE_DRIVE"
rm -rf "$GOOGLE_DRIVE_SYM"
ln -sf "$GOOGLE_DRIVE" "$GOOGLE_DRIVE_SYM" || true

[ ! -d "$ONE_DRIVE" ] && mkdir -pv "$ONE_DRIVE"
rm -rf "$ONE_DRIVE_SYM"
ln -sf "$ONE_DRIVE" "$ONE_DRIVE_SYM" || true

printf 'Setting up Homebrew/opt\n'
HOMEBREW_OPT=$HOMEBREW/.opt
HOMEBREW_OPT_SYM=$HOMEBREW/opt
[ ! -d "$HOMEBREW_OPT" ] && mkdir -pv "$HOMEBREW_OPT"
rm -rf "$HOMEBREW_OPT_SYM"
ln -sf "$HOMEBREW_OPT" "$HOMEBREW_OPT_SYM" || true

# Setting up Orgmode Agenda directory
printf 'Setting up Orgmode directory\n'
ORGMODE_DIR="$HOME/.org_agenda"
ORGMODE_DIR_SYM="$HOME/Org"
[ ! -d "$ORGMODE_DIR" ] && mkdir -pv "$ORGMODE_DIR"
rm -rf "$ORGMODE_DIR_SYM"
ln -sf "$ORGMODE_DIR" "$ORGMODE_DIR_SYM"

append_string() {
	if ! grep -Fxq "${2}" "${1}"; then
		# printf 'Appending %s with %s\n' "${2}" "${1}"
		printf "\n\n%s\n" "${2}" >>"${1}"
	fi
}
append_source() {
	append_string "${1}" ". ${2}"
}

printf 'Setting up shell environments\n'
# Importing bash settings
DOTFILESDIR="$SETTINGS_DIR/dotfiles"
LINUXBASHFILE="$DOTFILESDIR/bashrc_linux"
LINUXZSHFILE="$DOTFILESDIR/zshrc_linux"
DARWINBASHFILE="$DOTFILESDIR/bash_profile_osx"
# DARWINZSHFILE="$DOTFILESDIR/zshrc_osx"
# SHELL_TYPE="$(echo $SHELL)"

inst_env_linux() {
	[ ! -f "$HOME/.bashrc" ] && touch "$HOME/.bashrc"
	append_source "$HOME/.bashrc" "$LINUXBASHFILE"

	[ ! -f "$HOME/.zshrc" ] && touch "$HOME/.zshrc"
	append_source "$HOME/.zshrc" "$LINUXZSHFILE"
}

inst_env_cygwin() {
	append_source "$HOME/.bashrc" "$LINUXBASHFILE"
}

inst_env_darwin() {
	[ ! -f "$HOME/.bash_profile" ] && touch "$HOME/.bash_profile"
	append_source "$HOME/.bash_profile" "$DARWINBASHFILE"

	[ ! -f "$HOME/.zshrc" ] && touch "$HOME/.zshrc"
	append_source "$HOME/.zshrc" "$DARWINBASHFILE"
}

#
# Now install the shell environments!!
#
case "$PLATFORM" in
"linux")
	inst_env_linux
	;;

"cygwin")
	inst_env_cygwin
	;;

"darwin")
	inst_env_darwin
	;;

"*") # Considering general Unix OS as Linux
	inst_env_linux
	;;
esac

# Downloading unix_dev_setup
# --> Apparently, you have new enough git and other fileutils if you have already downloaded this file!
printf 'Setting up Unix Development Setup to %s\n' "$HOME/.uds"
UDS_DIR="$HOME/.uds"
UDS_LNK="$SETTINGS_DIR/unix_dev_setup"
if [ ! -d "$UDS_DIR" ]; then
	git clone "$MY_UDS_GIT_REPO" "$UDS_DIR"
	rm -rf "$UDS_LNK"
	ln -sf "$UDS_DIR" "$UDS_LNK"
fi

# Installing Rust stuffs
#printf 'Setting up Rust utilities.\n'
#if [ ! -d "$USR_DIR/.cargo" ]; then
#    /bin/bash -c "$SETTINGS_DIR/bin/setup_rust.sh"
#fi

printf '\n'
printf '\n**** Closing Comments ****\n'
printf 'Run %s dir to set up Doomemacs stuffs.\n\n' "$SETTINGS_DIR/bin/install_doomemacs.sh"
printf 'TODO: Also, don''t forget to populate %s\n\n' "$HOME/.gitconfig.local"
printf 'Have a nice day!\n\n'
