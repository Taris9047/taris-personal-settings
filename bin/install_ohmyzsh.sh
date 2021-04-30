#!/bin/sh
set -e

ZSH_CUSTOM="$HOME/.oh-my-zsh"

# Check if the system has zshell...
[ ! -x "$(command -v zsh)" ] && printf 'Zsh not found!\n' && exit 0

# Test purpose - Remove all the oh-my-zsh crap
rm -rf "$ZSH_CUSTOM" ~/.zshrc* 

# Installing Oh-my-zsh!
printf 'Installing Oh-my-zsh via curl!!\n'
[ ! -d "$ZSH_CUSTOM" ] && sh -c "ZSH= $(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --unattended"

# Some message
# printf '\n\n*** Ok, now type exit to install additional modules for Oh-my-Zsh!! ***\n'

# Zsh autosuggestions
git clone 'git://github.com/zsh-users/zsh-autosuggestions' "$ZSH_CUSTOM/plugins/zsh-autosuggestions"
git clone 'https://github.com/zsh-users/zsh-history-substring-search' "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search"
git clone 'https://github.com/zsh-users/zsh-syntax-highlighting.git' "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
git clone 'https://github.com/zsh-users/zsh-completions' "${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions"

# Enable plugins
append_string() {
    if ! grep -Fxq "${2}" "${1}"; then
	# printf 'Appending %s with %s\n' "${2}" "${1}"
	printf '%s\n' "${2}" >>"${1}"
    fi
}

# Let's activate those add-ons!
rm -rf "$HOME/.zshrc" && touch "$HOME/.zshrc"
append_string "$HOME/.zshrc" '#!/usr/bin/env zsh'
append_string "$HOME/.zshrc" "source ${HOME}/.settings/dotfiles/zshrc_linux"

case "$0" in
    *"zsh") . "${HOME}/.zshrc"
esac
