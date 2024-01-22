#!/bin/sh

WIN_USR="$(/mnt/c/Windows/System32/cmd.exe "/c" "echo %USERNAME%" | tr -d '\r')"

echo "Windows User: ${WIN_USR}"

# Adding Windows VSCode path to WSL shell...
code_path="/mnt/c/Users/${WIN_USR}/AppData/Local/Programs/Microsoft VS Code/bin"
code_cmd="${code_path}/code"

#echo "${code_cmd}"

if [ -f "${code_cmd}" ]; then
  printf 'VSCode found at: %s\n' "${code_cmd}"
  #alias code="$code_cmd"
  export PATH="$PATH:${code_path}"
  printf 'Now VS Code can be run with "code" command.\n'
fi

# Adding Windows Sublime Text path to WSL shell
subl_path="/mnt/c/Program Files/Sublime Text 3"
subl_cmd="${subl_path}/subl.exe"

if [ -f "${subl_cmd}" ]; then
  printf 'Sublime Text 3 found at: %s\n' "${subl_cmd}"
  export PATH="$PATH:${subl_path}"
  alias subl="\"${subl_cmd}\""
  printf 'Now Sublime Text 3 can be run with "subl" command.\n'
fi

# Cleaning up PATH
# referenced: 
# https://stackoverflow.com/a/11927340 
#
export PATH="$(echo "$PATH" |/bin/awk 'BEGIN{RS=":";}{sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}')";