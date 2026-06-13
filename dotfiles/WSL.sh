#!/bin/sh

check_symbol="\033[1;32m\u2713\033[0m"
right_arrow_symbol="\033[1;37m\u2192\033[0m"

printf '\n%b WSL environment detected!! Activating it ...\n' "${check_symbol}"

WIN_USR="$(/mnt/c/Windows/System32/cmd.exe "/c" "echo %USERNAME%" 2>/dev/null | tr -d '\r')"

printf '%b Windows User: %s\n' "${check_symbol}" "${WIN_USR}"

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

#
# Ollama stuffs
#
if [ -f "${HOME}/.openclaw/completions/openclaw.bash" ]; then
  . "${HOME}/.openclaw/completions/openclaw.bash"
fi
# Linuxbrew check
if [ -x "$(command -v /home/linuxbrew/.linuxbrew/bin/brew)" ]; then
  printf '%b Homebrew detected, activating it.\n' "${check_symbol}"
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv bash)"
else
  # Install homebrew
  printf '%b Homebrew not detected, installing it.\n' "${check_symbol}"
  sudo apt-get update 
  sudo apt-get install build-essential procps curl file git
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv bash)"
fi
# Winhost IP stuffs...
export WIN_HOST_IP=$(ip route | grep default | awk '{print $3}')
printf '%b Windows Host IP discovered: %s\n' "${check_symbol}" "${WIN_HOST_IP}"
# default Ollama port
OLLAMA_PORT='11434'
if [ ! -x "$(command -v socat)" ]; then
  printf '%b Socat is not on the system. Installing it\n' "${check_symbol}"
  sudo apt-get update && sudo apt-get install -y socat
fi
if lsof -Pi :${OLLAMA_PORT} -sTCP:LISTEN -t >/dev/null ; then
  printf '%b Port %s is already in use. Skipping socat execution\n' "${check_symbol}" "${OLLAMA_PORT}"
else
  # Run the socat port mapping for external ollama
  printf '%b Running socat to map the IP port for Ollama\n' "${check_symbol}"
  socat TCP-LISTEN:${OLLAMA_PORT},reuseaddr,fork TCP:${WIN_HOST_IP}:${OLLAMA_PORT} &
fi



# Cleaning up PATH
# referenced: 
# https://stackoverflow.com/a/11927340 
#
export PATH="$(echo "$PATH" |/bin/awk 'BEGIN{RS=":";}{sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}')";
