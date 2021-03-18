# Fish config file...
#

### EXPORT ###
# set HOMEBREW $HOME/.local/bin
# echo "Setting homebrew dir to $HOMEBREW"
# set -Ua fish_user_paths $HOME/.local/bin $fish_user_paths
set fish_greeting
set TERM "xterm-256color"

if type -q nvim
    set EDITOR "nvim"
else
    set EDITOR "vi"
end

if type -q subl
    set VISUAL "subl"
else
    set VISUAL "xdg-open"
end

### Setting up manpager ###
if type -q bat
    set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"
else if type -q vim and not type -q nvim
    set -x MANPAGER '/bin/bash -c "vim -MRn -c \"set buftype=nofile showtabline=0 ft=man ts=8 nomod nolist norelativenumber nonu noma\" -c \"normal L\" -c \"nmap q :qa<CR>\"</dev/tty <(col -b)"'
else if type -q nvim
    set -x MANPAGER "nvim -c 'set ft=man' -"
end

### Some utility paths ###
set GOOGLE_DRIVE $HOME/.google-drive
set ONE_DRIVE $HOME/.onedrive
set texlive_base_path $HOME/.texlive
set check_symbol "\033[1;32m\u2713\033[0m"
set right_arrow_symbol "\033[1;37m\u2192\033[0m"
set line_delay 0.12

### Prepend path
function addpath
    set -a fish_user_paths $argv[1]
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
if type -q broot
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
function gitlog2w
    for day in (seq 14 -1 0)
        git log --before="$day days" --after="($day+1) days" --format=oneline | wc -l
    end | spark
end
function gitlog8h
    for hour in (seq 8 -1 0)
        git log --before="$hour hours" --after="($hour+1) hours" --format=oneline | wc -l
    end | spark
end


### Switch between shells ###
alias tobash="sudo -H chsh $USER -s bash && echo 'Now log out.'"
if type -q zsh
    alias tozsh="sudo -H chsh $USER -s zsh && echo 'Now log out.'"
end
if type -q fish
    alias tofish="sudo -H chsh $USER -s fish && echo 'Now log out.'"
end

### Termbin ###
alias tb="nc termbin.com 9999"

### 'open' macro. Just like OS X ###
function open
    if type -q xdg-open
        xdg-open "$argv[1]" &>/dev/null &
    else
        echo "open on this kind of file has not implemented yet!"
    end
end

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
if test -d "$HOME/.local"
    set HOMEBREW "$HOME/.local"
    printf "$check_symbol HOMEBREW directory is $HOMEBREW\n"
    sleep $line_delay
    set -a fish_user_paths "$HOMEBREW/bin"
    set -a fish_user_paths "$HOMEBREW/.opt/bin"
end

# JRE
set JAVA_HOME '/opt/java'
if test -d "$JAVA_HOME"
    printf "$check_symbol Java found at $JAVA_HOME\n"
    sleep $line_delay
    addpath "$JAVA_HOME/bin"
end

# Emacs
set EMACS_HOME $HOME/.emacs_local
if test -d "$EMACS_HOME"
    printf "$check_symbol Custom emacs found at $EMACS_HOME\n"
    sleep $line_delay
    addpath "$EMACS_HOME/bin"
end

# Rust
if test -d "$HOME/.cargo"
    printf "$check_symbol Cargo directory detected at $HOME/.cargo\n"
    sleep $line_delay
    addpath "$HOME/.cargo/bin"
end

# GCC variants.
if test -d "$HOMEBREW/.opt/gcc-jit"
    printf "$check_symbol Gcc with libgccjit found in the system!\n"
    addpath "$HOMEBREW/.opt/gcc-jit/bin"
end
if test -d "$HOMEBREW/.opt/gcc9"
    printf "$check_symbol Gcc9 found in the system!\n"
    addpath "$HOMEBREW/.opt/gcc9/bin"
end
if test -d "$HOMEBREW/.opt/gcc8"
    printf "$check_symbol Gcc8 found in the system!\n"
    addpath "$HOMEBREW/.opt/gcc8/bin"
end
if test -d "$HOMEBREW/.opt/gcc4"
    printf "$check_symbol Gcc4 found in the system!\n"
    addpath "$HOMEBREW/.opt/gcc4/bin"
end

# Golang
set GOROOT "$HOMEBREW/.opt/go"
set GOPATH "$HOMEBREW/.opt/go/bin"
if test -d "$GOROOT"
    printf "$check_symbol Golang has been found at $GOROOT\n"
    addpath "$GOROOT/bin"
end

# snap
set SNAP_BIN /snap/bin
if test -d "$SNAP_BIN"
    printf "$check_symbol Snap executables have been found at $SNAP_BIN\n"
    addpath "$SNAP_BIN"
end

# VNC
if type -q vncserver
    echo "$check_symbol VNC server found!"
    echo "  To start: vncstart"
    echo "  To end: vnckill"
    alias vncstart="vncserver -localhost no -useold -geometry 1200x800 -depth 32"
    alias vnckill="vncserver -kill :1"
end

# exa
if type -q exa
    printf "$check_symbol exa found! using it instead of ls\n"
    sleep $line_delay
    alias ls='exa -hF --color=always --group-directories-first'
    alias ll='exa -lahF --color=always --group-directories-first'
    alias l='exa -hF --color=always --group-directories-first'
    alias lt='exa -aT --color=always --group-directories-first'
    alias l.='exa -a | egrep "^\."'
end

# lsd
if type -q lsd
    printf "$check_symbol lsd found! using it instead of ls or exa\n"
    sleep $line_delay
    alias ls='lsd -hF --color=always --group-dirs=first'
    alias ll='lsd -lahF --color=always --group-dirs=first'
    alias lld='lsd -lahF --color=always --group-dirs=first --total-size'
    alias l='lsd -hF --color=auto --group-dirs=first'
    alias lt='lsd -a --tree --color=always --group-dirs=first'
    alias l.='lsd -a | egrep "^\."'
end

# bat
if type -q bat
    printf "$check_symbol bat found! using it instead of cat\n"
    sleep $line_delay
    alias cat='bat'
end

# rip, rm-improved
set trash_location "$HOME/.local/share/Trash/files/"
if type -q
    printf "$check_symbol rip, rm-improved found!\n $right_arrow_symbol Setting up graveyard at $trash_location"
    sleep $line_delay
    alias rip="rip --graveyard $trash_location"
end

# bpytop
if type -q $HOMEBREW/bin/pip3
    and type -q $HOMEBREW/bin/bpytop
    printf "$check_symbol Locally installed bpytop found!\n"
    sleep $line_delay
    alias bpytop="$HOMEBREW/bin/pip3 install -U bpytop && $HOMEBREW/bin/bpytop"
    alias htop="$HOMEBREW/bin/bpytop"
    alias top="$HOMEBREW/bin/bpytop"
end

# zoxide
if type -q zoxide
    printf "$check_symbol zoxide found! activating it! (z replaces cd)\n"
    sleep $line_delay
    zoxide init fish | source
    # On fish, cd has an alias! So, using yet another alias to alias is not a good idea!
    # Just use z instead of cd
    # alias cd='z'
end

# neovim
if type -q nvim
    printf "$check_symbol Neovim found! replacing vim!\n"
    sleep $line_delay
    alias vim='nvim'
    alias vi='nvim'
end

# Texlive
set texlive_year "2020"
set texlive_arch "x86_64-linux"
set texlive_bin_dir "$texlive_base_path/$texlive_year/bin/$texlive_arch"
set texlive_bin_dir_woyear "$texlive_base_path/bin/$texlive_arch"
if test -d "$texlive_bin_dir"
    printf "$check_symbol Texlive found at $texlive_bin_dir directory!!\n"
    addpath "$texlive_bin_dir"
else if test -d "$texlive_bin_dir_woyear"
    printf "$check_symbol Texlive found at $texlive_bin_dir_woyear directory!!\n"
    addpath "$texlive_bin_dir_woyear"
end

# pypy - brewed one
if type -q "$HOMEBREW/.opt/pypy/bin/pypy3"
    printf "$check_symbol pypy3 found in $HOMEBREW/.oopt/pypy/bin/pypy3"
    sleep $line_delay
    addpath "$HOMEBREW/.opt/pypy/bin"
end

# Node.JS's update tool, n
if type -q n
    printf "$check_symbol n found!, Setting up N_PREFIX for it!\n"
    sleep $line_delay
    set -U N_PREFIX (type -p n | sed -E 's/\/bin\/n//g')
end

# IrfanView - you need wine to install it!
set iview64_path "$HOME/.wine/drive_c/Program\ Files/IrfanView/i_view64.exe"
function run_iview
    wine "$iview64_path" (winepath --windows $argv[@])
end
if type -q wine
    and test -f "$iview64_path"
    printf "$check_symbol Irfanveiw found!\n"
    printf "  $right_arrow_symbol Usage: iview <files>\n"
    alias iview='run_iview'
    sleep $line_dealy
end

# youtube-dl stuff
if type -q youtube-dl
    printf "$check_symbol youtube-dl found! setting up yta(ytv)-* commands.\n"
    alias yta-help="echo 'yta-aac yta-best yta-flac yta-m4a yta-mp3 yta-opus yta-vorbis yta-wav ytv-best'"
    alias yta-aac="youtube-dl --extract-audio --audio-format aac "
    alias yta-best="youtube-dl --extract-audio --audio-format best "
    alias yta-flac="youtube-dl --extract-audio --audio-format flac "
    alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
    alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
    alias yta-opus="youtube-dl --extract-audio --audio-format opus "
    alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
    alias yta-wav="youtube-dl --extract-audio --audio-format wav "
    alias ytv-best="youtube-dl -f bestvideo+bestaudio "
    sleep $line_delay
end

# Rclone stuff!
if type -q rclone
    if not test -d "$GOOGLE_DRIVE"
        printf "$check_symbol Google drive mount point not found! making one...\n"
        mkdir -pv "$GOOGLE_DRIVE"
    end

    if grep -qs "$GOOGLE_DRIVE" /proc/mounts
        printf "$check_symbol Google drive already mounted at $GOOGLE_DRIVE\n"
        sleep $line_delay
    else
        if test -n (cat $HOME/.config/rclone/rclone.conf | grep "\[google-drive\]")
            printf "$check_symbol Mounting Google Drive to $GOOGLE_DRIVE\n"
            rclone mount google-drive: "$GOOGLE_DRIVE" &
            sleep 2
        end
    end

    if not test -d "$ONE_DRIVE"
        printf "$check_symbol MS One Drive mount point not found! making one...\n"
        mkdir -pv "$ONE_DRIVE"
    end

    if grep -qs "$ONE_DRIVE" /proc/mounts
        printf "$check_symbol MS One drive already mounted at $ONE_DRIVE\n"
        sleep $line_delay
    else
        if test -n (cat $HOME/.config/rclone/rclone.conf | grep "\[onedrive\]")
            printf "$check_symbol Mounting MS One Drive to $GOOGLE_DRIVE\n"
            rclone mount --vfs-cache-mode writes onedrive: $ONE_DRIVE &
            sleep 2
        end
    end
end

# ROOT
set ROOT_DIR "$HOMEBREW/.opt/ROOT"
if type -q "$ROOT_DIR/bin/root"
    printf "$check_symbol ROOT Found! Applying its shell env.\n"
    alias thisroot="$ROOT_DIR/bin/thisroot.fish"
end

# Doomemacs!
if test -d "$HOME/.emacs.d/bin"
    and test -d "$HOME/.doom.d"
    printf "$check_symbol Doomemacs found! Adding to path!\n"
    addpath "$HOME/.emacs.d/bin"
end

# old MBP cuda
if test -d "/usr/local/cuda-6.5"
    printf "$check_symbol CUDA 6.5 found! Doing some env stuff for it.\n"
    set -Ua LD_LIBRARY_PATH "/usr/local/cuda-6.5/lib64"
    set -Up fish_user_paths "/usr/local/cuda-6.5/bin"
end

# Starship
if type -q starship
    printf "$check_symbol Starship shell extension found! Let's start it!\n"
    sleep $line_delay
    starship init fish | source
end

# Clean up screen
if type -q spark and type -q lolcat
    alias clear='clear; echo; echo; seq 1 (tput cols) | sort -R | spark | lolcat; echo; echo' # Coloured
else if type -q spark and not type -q lolcat
    alias clear='clear; echo; echo; seq 1 (tput cols) | sort -R | spark | echo; echo' # Non-Coloured end clear
end

clear

# Finally, run neofetch
if type -q neofetch
    neofetch
end
