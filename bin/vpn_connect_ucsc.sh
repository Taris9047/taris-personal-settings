#!/bin/bash

VPN_SERVER="vpn.ucsc.edu"

if [ ! -f "$HOME/.vpn_creds" ]; then
  printf 'We are lacking .vpn_creds in the users home dir.\n'
  exit 1
fi

# VPN PATH
VPN="/opt/cisco/anyconnect/bin/vpn"

echo "Connectiong to UCSC Campus VPN"

# Prints simple usage function
usage() {
  printf 'usage: vpn_connect_ucsc.sh <options> <program>\n'
  printf 'options:\n'
  printf '-h or --help: Prints out this message.\n'
  printf '-f or --force: Disconnects vpn then try to reconnect\n'
}

# Parsing arguments
prog=''
while (($# > 0)); do
  case "$1" in
    -h | --help)
      usage
      exit 0
      ;;
    -f | --force)
      printf 'Forcefully disconnecting the VPN to re-establish the connection\n'
      "$VPN" disconnect
      ;;
    *)
      prog="$1"
      ;;
  esac
done



if [ -n "$1" ] && [ "$1" = "-f" ] || [ "$1" = "--force" ]; then
  printf 'Forcefully disconnecting the VPN to re-establish the connection\n'
  "$VPN" disconnect
fi

prog=''
if [ ! -z "$1" ]; then
  prog="$1"
fi

connect_ucsc_vpn () {

  # sleep 1
  
  if [ ! -z "$prog" ]; then
    prog_ps="$(ps -A | grep "$prog")"
    if [ -z "$prog_ps" ]; then
      printf 'Not running the progress "%s" in background.\n' "$prog"
      printf 'Thus, no reason to connect to VPN again!\n'
      exit 0
    fi
  fi
    
  if [ -n "$("${VPN}" state | grep -i 'Disconnected')" ]; then
    "$VPN" -s < "$HOME/.vpn_creds" connect "${VPN_SERVER}" || connect_ucsc_vpn
  else
    printf 'It seems we are still on the VPN!\n'
    exit 0
  fi
}; connect_ucsc_vpn

# Restart the VNC server...
SCRIPT_PATH="$HOME/.settings/bin/vncserver_restart.sh"
if [ "$(id -u)" -eq 0 ]; then
  printf 'Restarting RealVNC Server\n'
  sudo $SCRIPT_PATH
fi

