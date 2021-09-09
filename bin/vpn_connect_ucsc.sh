#!/bin/sh

VPN_SERVER="vpn.ucsc.edu"

if [ ! -f "$HOME/.vpn_creds" ]; then
  printf 'We are lacking .vpn_creds in the user's home dir.\n'
  exit 1
fi

echo "Connectiong to UCSC Campus VPN"

# VPN PATH
VPN="/opt/cisco/anyconnect/bin/vpn"

"$VPN" disconnect
sleep 1
"$VPN" -s < "$HOME/.vpn_creds" connect "${VPN_SERVER}"

