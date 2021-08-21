#!/bin/sh

VPN_SERVER="vpn.ucsc.edu"

echo "Connectiong to UCSC Campus VPN"

# VPN PATH
VPN="/opt/cisco/anyconnect/bin/vpn"

"$VPN" disconnect
sleep 1
"$VPN" -s < $HOME/.vpn_creds connect "${VPN_SERVER}"

