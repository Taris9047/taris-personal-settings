# Fish config file...
#

# TODO Uhhh shit... fish is totally different animal! Let's ranslate my_settings to fish
#
# Some guidelines:
# 
#    setting variables
#        bash: var=value
#        fish: set var value
#    function arguments
#     bash: "$@"
#     fish: $argv
# function local variables
#     bash: local var
#     fish: set -l var
# conditionals I
#     bash: [[ ... ]] and [ ... ]
#     fish: test ...
# conditionals II
#     bash: if cond; then cmds; fi
#     fish: if cond; cmds; end
# conditionals III
#     bash: cmd1 && cmd2
#     fish: cmd1; and cmd2
#     fish (as of fish 3.0): cmd1 && cmd2
# command substitution
#     bash: output=$(pipeline)
#     fish: set output (pipeline)
# process substitution
#     bash: join <(sort file1) <(sort file2)
#     fish: join (sort file1 | psub) (sort file2 | psub)

### EXPORT ###
set HOMEBREW $HOME/.local/bin
echo "Setting homebrew dir to $HOMEBREW"
set -U fish_user_paths $HOME/.local/bin $fish_user_paths
set fish_greeting
set TERM "xterm-256color"
set EDITOR "nvim"
set VISUAL "subl"

### Some utility paths ###
set HBREW_PATH $HOME/.local
set GOOGLE_DRIVE $HOME/.google-drive
set ONE_DRIVE $HOME/.onedrive
set texlive_base_path $HOME/.texlive
set check_symbol "\033[1;32m\u2713\033[0m"
set right_arrow_symbol "\033[1;37m\u2192\033[0m"
set line_delay 0.12

### Prepend path
function addpath
    set PATH $argv[1] $PATH
end

### Set manpager ###
# not yet

### AUTOCOMPLETE AND HIGHLIGHT COLORS ###
set fish_color_normal brcyan
set fish_color_autosuggestion '#7d7d7d'
set fish_color_command brcyan
set fish_color_error '#ff6c6b'
set fish_color_param brcyan

### Basic Aliases ###
alias ls="ls -pFh --color=auto --show-control-chars"
alias ll="ls -la"
alias l="ls"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias df="df -h"
alias rsync="rsync -azvh --info=progress2"
alias sudo="sudo -H"
alias free="free -m"

### Aliases for other tools ###
if test -x broot
    alias br="broot -dhp"
    alias bs="broot --sizes"
end

### Alises for directory nav. ###
alias 'cd..'="cd .."
alias '...'="cd ../.."
alias '....'="cd ../../.."
alias '.....'="cd ../../../.."

#
# A few more aliases
#
alias aptup='sudo apt-get -y update && sudo apt-get -y upgrade'
alias aptin='sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get install'
alias dnfup='sudo dnf -y update'
alias dnfin='sudo dnf -y install'
alias pmyy='sudo pacman -Syyu'
alias pmin='sudo pacman -Syyu'
## get top process eating memory
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
## get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'
# gpg encryption
# verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
# receive the key of a developer
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"

### Git related stuffs ###
function gc
    git commit -a -m "\"$argv[1]\"" && git push
end
function gcatchup
    git fetch --all && git reset --hard origin/master && git pull
end
function gtag
    git tag -a "\"$argv[1]\""
end
alias gaddup='git add -u'
alias gaddall='git add .'

### Switch between shells ###
alias tobash="sudo -H chsh $USER -s /bin/bash && echo 'Now log out.'"
if test -x /bin/zsh
    alias tozsh="sudo -H chsh $USER -s /bin/zsh && echo 'Now log out.'"
end

### Termbin ###
alias tb="nc termbin.com 9999"

### Application Aliases ###
if test -x 'nvim'
    echo "neovim not found int the path!"
end
alias vim='nvim'

### Extraction ###
function ex
    if test -f $argv[1]
        switch $argv[1]
            case '*.tar.bz2'
                tar xjf $argv[1]
            case '*.tar.gz'
                tar xzf $argv[1]
            case '*.bz2'
                bunzip2 $argv[1]
            case '*.rar'
                unrar x $argv[1]
            case '*.gz'
                gunzip $argv[1]
            case '*.tar'
                tar xf $argv[1]
            case '*.tbz2'
                tar xjf $argv[1]
            case '*.tgz'
                tar xzf $argv[1]
            case '*.zip'
                unzip $argv[1]
            case '*.Z'
                uncompress $argv[1]
            case '*.7z'
                7z x $argv[1]
            case '*.deb'
                ar x $argv[1]
            case '*.tar.xz'
                tar xf $argv[1]
            case '*.tar.zst'
                unzstd $argv[1]
            case '*'
                echo "$argv[1] cannot be extracted via ex() yet."
        end
    else
        echo "$argv[1] is not a valid file!"
    end
end

### Navigate ###
function up
    set d ""
    set limit "$argv[1]"

    if test -z "$limit" -o "$limit" -le 0
        set limit 1
    end

    for i in (seq 0 "$limit")
        set d "../$d"
    end

    if ! cd "$d"
        echo "Couldn't go up $limit dirs."
    end
end

### Now path things... a lot of them ###

# Homebrew path
if test -d "$HBREW_PATH"
    set HOMEBREW $HBREW_PATH
    printf "$check_symbol HOMEBREW directory is $HOMEBREW\n"
    sleep $line_delay
    set PATH "$HOMEBREW/bin" "$HOMEBREW/.opt/bin" $PATH
end

# JRE
set JAVA_HOME '/opt/java'
if test -d "$JAVA_HOME"
    printf "$check_symbol Java found at $JAVA_HOME\n"
    sleep $line_delay
    fish_add_path "$JAVA_HOME"
end

# Emacs
set EMACS_HOME $HOME/.emacs_local
if test -d $EMACS_HOME
    printf "$check_symbol Custom emacs found at $EMACS_HOME\n"
    sleep $line_delay
    fish_add_path "$EMACS_HOME/bin"
end

# Rust
if test -d $HOME/.cargo
    printf "$check_symbol Cargo directory detected at $HOME/.cargo\n"
    sleep $line_delay
    set PATH "$HOME/.cargo/bin" $PATH
end

# GCC variants.
if test -d $HOMEBREW/.opt/gcc-jit
    printf "$check_symbol Gcc with libgccjit found in the system!\n"
    fish_add_path "$HOMEBREW/.opt/gcc-jit/bin"
end
if test -d $HOMEBREW/.opt/gcc9
    printf "$check_symbol Gcc9 found in the system!\n"
    fish_add_path "$HOMEBREW/.opt/gcc9/bin"
end
if test -d $HOMEBREW/.opt/gcc8
    printf "$check_symbol Gcc8 found in the system!\n"
    fish_add_path "$HOMEBREW/.opt/gcc8/bin"
end
if test -d $HOMEBREW/.opt/gcc4
    printf "$check_symbol Gcc4 found in the system!\n"
    fish_add_path "$HOMEBREW/.opt/gcc4/bin"
end

# Golang
set GOROOT "$HOMEBREW/.opt/go"
set GOPATH "$HOMEBREW/.opt/go/bin"
if test -d $GOROOT
    printf "$check_symbol Golang has been found at $GOROOT\n"
    fish_add_path "$GOROOT/bin"
end

# Starship
if type -q starship
    printf "$check_symbol Starship shell extension found! Let's start it!\n"
    sleep $line_delay
    starship init fish | source
end

# Clean up screen
clear

# Finally, run neofetch
if type -q neofetch
    neofetch
end
