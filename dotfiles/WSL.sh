#!/bin/sh

check_symbol="\033[1;32m\u2713\033[0m"

printf '\n%b WSL environment detected!! Activating it ...\n' "${check_symbol}"

WIN_USR="$(/mnt/c/Windows/System32/cmd.exe "/c" "echo %USERNAME%" 2>/dev/null | tr -d '\r')"

printf '%b Windows User: %s' "${check_symbol}" "${WIN_USR}"

# Adding Windows VSCode path to WSL shell...
code_path="/mnt/c/Users/${WIN_USR}/AppData/Local/Programs/Microsoft VS Code/bin"
code_cmd="${code_path}/code"

#echo "${code_cmd}"

if [ -f "${code_cmd}" ]; then
  printf '%b VSCode found at: %s\n' "${check_symbol}" "${code_cmd}"
  #alias code="$code_cmd"
  export PATH="$PATH:${code_path}"
  printf '%b Now VS Code can be run with "code" command.\n' "${check_symbol}"
fi

# Adding Windows Sublime Text path to WSL shell
subl_path="/mnt/c/Program Files/Sublime Text 3"
subl_cmd="${subl_path}/subl.exe"

if [ -f "${subl_cmd}" ]; then
  printf '%b Sublime Text 3 found at: %s\n' "${check_symbol}" "${subl_cmd}"
  export PATH="$PATH:${subl_path}"
  alias subl="\"${subl_cmd}\""
  printf '%b Now Sublime Text 3 can be run with "subl" command.\n' "${check_symbol}"
fi

# Adding IrfanView into WSL path.
iview_path="/mnt/c/Program Files/IrfanView"
iview64_cmd="${iview_path}/i_view64.exe"
iview32_cmd="${iview_path}/i_view32.exe"

if [ -f "${iview32_cmd}" ]; then
  printf '%b IrfanView 32bit found at: %s\n' "${check_symbol}" "${iview32_cmd}"
  alias iview="\"${iview32_cmd}\""
  printf '%b Now you can run IrfanView with "iview" command.\n' "${check_symbol}"
elif [ -f "${iview64_cmd}" ]; then
  printf '%b IrfanView 64bit found at: %s\n' "${check_symbol}" "${iview64_cmd}"
  alias iview="\"${iview64_cmd}\""
  printf '%b Now you can run IrfanView with "iview" command.\n' "${check_symbol}"
fi






# Cleaning up PATH
# referenced: 
# https://stackoverflow.com/a/11927340 
#
export PATH="$(echo "$PATH" |/bin/awk 'BEGIN{RS=":";}{sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}')";
