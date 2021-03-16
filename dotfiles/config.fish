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

### Set manpager ###
# not yet

### AUTOCOMPLETE AND HIGHLIGHT COLORS ###
set fish_color_normal brcyan
set fish_color_autosuggestion '#7d7d7d'
set fish_color_command brcyan
set fish_color_error '#ff6c6b'
set fish_color_param brcyan

### SPARK ###
set -g spark_version 1.0.0

complete -xc spark -n __fish_use_subcommand -a --help -d "Show usage help"
complete -xc spark -n __fish_use_subcommand -a --version -d "$spark_version"
complete -xc spark -n __fish_use_subcommand -a --min -d "Minimum range value"
complete -xc spark -n __fish_use_subcommand -a --max -d "Maximum range value"

function spark -d "sparkline generator"
    if isatty
        switch "$argv"
            case {,-}-v{ersion,}
                echo "spark version $spark_version"
            case {,-}-h{elp,}
                echo "usage: spark [--min=<n> --max=<n>] <numbers...>  Draw sparklines"
                echo "examples:"
                echo "       spark 1 2 3 4"
                echo "       seq 100 | sort -R | spark"
                echo "       awk \\\$0=length spark.fish | spark"
            case \*
                echo $argv | spark $argv
        end
        return
    end

    command awk -v FS="[[:space:],]*" -v argv="$argv" '
        BEGIN {
            min = match(argv, /--min=[0-9]+/) ? substr(argv, RSTART + 6, RLENGTH - 6) + 0 : ""
            max = match(argv, /--max=[0-9]+/) ? substr(argv, RSTART + 6, RLENGTH - 6) + 0 : ""
        }
        {
            for (i = j = 1; i <= NF; i++) {
                if ($i ~ /^--/) continue
                if ($i !~ /^-?[0-9]/) data[count + j++] = ""
                else {
                    v = data[count + j++] = int($i)
                    if (max == "" && min == "") max = min = v
                    if (max < v) max = v
                    if (min > v ) min = v
                }
            }
            count += j - 1
        }
        END {
            n = split(min == max && max ? "▅ ▅" : "▁ ▂ ▃ ▄ ▅ ▆ ▇ █", blocks, " ")
            scale = (scale = int(256 * (max - min) / (n - 1))) ? scale : 1
            for (i = 1; i <= count; i++)
                out = out (data[i] == "" ? " " : blocks[idx = int(256 * (data[i] - min) / scale) + 1])
            print out
        }
    '
end
### END OF SPARK ###


### Basic Aliases ###
alias sudo="sudo -H"

### Application Aliases ###
if test -x 'nvim';
    echo "neovim not found int the path!"
end
alias vim='nvim'