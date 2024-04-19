#!/bin/bash
#
# Runs NoIP-DUC with script...
#

DUC_KEY="${HOME}/.noip-duc-keys"

write_NoIP-DUC_key() {
  if [ ! -f "${DUC_KEY}" ]; then
    touch "${DUC_KEY}"
  fi

  read -p 'NoIP-DUC ID: ' duc_id
  printf '%s\n' "${duc_id}" | tee -a "${DUC_KEY}" > /dev/null
  read -sp 'NoIP-DUC PW: ' duc_pass
  if [ -x "$(command -v base64)" ]; then
    printf '%s\n' "${duc_pass}" | base64 - | tee -a "${DUC_KEY}" > /dev/null
  else
    printf '%s\n' "${duc_pass}" | tee -a "${DUC_KEY}" > /dev/null
  fi
  printf '%s\n' 'all.ddnskey.com' | tee -a "${DUC_KEY}" > /dev/null

  printf '%s generated!!\n' "${DUC_KEY}" 
}

if [ ! -f "${DUC_KEY}" ]; then
  printf 'No No-IP DUC key information found!! Generating one\n'
  write_NoIP-DUC_key
fi

if [ ! -x "$(command -v noip-duc)" ]; then
  printf 'No-IP DUC program is not found in the system!! Exiting the script!\n'
  exit 1
fi

printf 'Running to No-IP DUC'
DUC_EXE="$(command -v noip-duc)"

run_NoIP-DUC() {

  tmp_file="$(mktemp)"
  declare -a array=()
  i=0
  while IFS= read -r line; do
    array[i]="${line}"
    let "i++"
  done <"${DUC_KEY}"
  
  # Writing decoded info into temporary file
  printf '%s\n' "${array[0]}" | tee -a "${tmp_file}" > /dev/null
  if [ -x "$(command -v base64)" ]; then
    printf '%s\n' "${array[1]}" | base64 --decode | tee -a "${tmp_file}" > /dev/null
  else
    printf '%s\n' "${array[1]}" | tee -a "${tmp_file}" > /dev/null
  fi
  printf '%s\n' "${array[2]}" | tee -a "${tmp_file}" > /dev/null

  declare -a key_data=()
  i=0
  while IFS= read -r line; do
    key_data[i]="${line}"
    let "i++"
  done <"${tmp_file}"

  # Finally running the NoIP-DUC
  "${DUC_EXE}" --username "${key_data[0]}" --password "${key_data[1]}" -g "${key_data[2]}"

  rm -rf "${tmp_file}"
}

run_NoIP-DUC
