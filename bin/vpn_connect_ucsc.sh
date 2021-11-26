#!/bin/sh

VPN_SERVER="vpn.ucsc.edu"

if [ ! -f "$HOME/.vpn_creds" ]; then
  printf 'We are lacking .vpn_creds in the users home dir.\n'
  exit 1
fi

echo "Connectiong to UCSC Campus VPN"

# VPN PATH
VPN="/opt/cisco/anyconnect/bin/vpn"

"$VPN" disconnect

connect_ucsc_vpn (){
  sleep 1
  "$VPN" -s < "$HOME/.vpn_creds" connect "${VPN_SERVER}" || connect_ucsc_vpn
}; connect_ucsc_vpn

# Restart the VNC server...
SCRIPT_PATH="$HOME/.settings/bin/vncserver_restart.sh"
if [ "$(id -u)" -eq 0 ]; then
  printf 'Restarting RealVNC Server\n'
  sudo $SCRIPT_PATH
fi

