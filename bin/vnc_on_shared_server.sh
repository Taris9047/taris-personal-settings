#!/bin/bash

unalias vncstart 2> /dev/null
unalias vnckill 2> /dev/null

# VNC server resolution
VNC_GEOM='1760x990'

if [ -x "$(command -v vncstart)" ]; then
  printf 'Xtightvnc is not found!\n'
  exit 1
fi

# ports to ignore: 5901-10

# Tightvnc case

# Preferred port: 5907
VNCPORT=7

all_vnc_servers="$(ps -ef | grep 'Xtightvnc' | awk 'match($0, / :[0-9]+/) {print substr( $0, RSTART, RLENGTH)}')"
my_vnc_servers="$(ps -ef | grep 'Xtightvnc' | awk 'match($0, / :[0-9]+/) {print substr( $0, RSTART, RLENGTH)}')"

#echo "${my_vnc_servers}"

# Start XtightVNC server at given port
# Say if :7 is given, it opens a server at 5907
vncstart() {
  if [[ "${my_vnc_servers}" == *"${1}"* ]]; then
    printf 'VNCPort %s is already in use by yourself.\n' "${1}"
    exit 1
  else
    printf 'Opening VNCServer at VNCPort %s\n' "${1}"
    vncserver "${1}" -geometry "${VNC_GEOM}" -depth 24
  fi
}

vnckill() {
  if [[ "${my_vnc_servers}" == *"${1}"* ]]; then
    printf 'VNCPort %s is your vnc screen. Will you kill it? [y/N] ' "${1}"
    read answer

    if [ "${answer}" != "${answer#[Yy]}" ] ;then 
      printf 'Killing VNCServer@Port %s' "${1}"
      vncserver -kill "${1}"
    else
      printf 'Not killing VNCServer@Port %s' "${1}"
    fi
  else
    printf 'VNCPort %s is not your own server! Everyone has rights!\n' "${1}"
    exit 1
  fi
}

vnc_start_default() {
  printf 'Trying to open a VNCServer at Port :%s\n' "${VNCPORT}"
  if [[ "${all_vnc_servers}" != *"${VNCPORT}"* ]]; then
    vncstart "${VNCPORT}"
  else
    printf 'An XtightVNC server is running at VNCPort :%s\n' "${VNCPORT}"
    printf 'Please kill it if it was yours. And then run this script again.\n'
    exit 0
  fi
}

# The help
disp_help() {
  printf 'Usage: \n'
  printf ' <noarg>: Opens an Xtightvnc server at Port :%s\n' "${VNCPORT}"
  printf ' -p: Opens an Xtightvnc server at given port. For example, -p :3 for 5903\n'
  printf ' -k: Kills Xtightvnc server at given port. For example, -k :3\n'
  printf ' -h: Displays this message\n'
}


# The option stuffs...
[ $# -eq 0 ] && vnc_start_default && exit 0

while getopts "hk:p:" option; do
  case "${option}" in
    h)
      disp_help
      exit 0
      ;;
    k)
      vnckill "${OPTARG}"
      ;;
    p)
      vncstart "${OPTARG}"
      ;;
  esac
done
