#!/bin/bash

VPN_SERVER="vpn.ucsc.edu"
VPN_CREDS="$HOME/.vpn_creds"

write_vpn_creds () {
    touch "$VPN_CREDS"
    read -p 'VPN Username: ' username
    echo "$username" | tee -a "$VPN_CREDS" > /dev/null
    read -sp 'VPN (Gold) Password: ' userpass
    if [ -x "$(command -v base64)" ]; then
        echo "$userpass" | base64 - | tee -a "$VPN_CREDS" > /dev/null
    else
        echo "$userpass" | tee -a "$VPN_CREDS" > /dev/null
    fi
    # and finally push..
    echo "push" | tee -a "$VPN_CREDS" > /dev/null
    printf '%s generated!\n' "$VPN_CREDS"
}

if [ ! -f "$VPN_CREDS" ]; then
    printf 'We are lacking .vpn_creds in the users home dir.\n'
    write_vpn_creds
fi

# VPN PATH
VPN_ANYCONNECT="/opt/cisco/anyconnect/bin/vpn"
VPN_SECURECLIENT="/opt/cisco/secureclient/bin/vpn"

if [ -f "$VPN_ANYCONNECT" ]; then
    VPN="$VPN_ANYCONNECT"
elif [ -f "$VPN_SECURECLIENT" ]; then
    VPN="$VPN_SECURECLIENT"
fi

if [ -f "$VPN" ]; then
    printf 'VPN client detected at %s\n' "$VPN"
else
    printf 'VPN client has not found! Install Cisco VPN first!\n'
    exit -1
fi

echo "Connecting to UCSC Campus VPN"

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
            shift
            ;;
        -d | --disconnect)
            printf 'Disconnecting from VPN\n'
            "$VPN" disconnect
            exit 0
            ;;
        *)
            prog="$1"
            shift
            ;;
    esac
done

if [ -n "$1" ] && [ "$1" = "-f" ] || [ "$1" = "--force" ]; then
    printf 'Forcefully disconnecting the VPN to re-establish the connection\n'
    "$VPN" disconnect
fi

# Restart the VNC server...
restart_realvnc_server () {
    SCRIPT_PATH="$HOME/.settings/bin/vncserver_restart.sh"
    if [ "$(id -u)" -eq 0 ]; then
        printf 'Restarting RealVNC Server\n'
        $SCRIPT_PATH
    fi
}

connect_ucsc_vpn() {

    # sleep 1

    if [ ! -z "$prog" ]; then
        prog_ps="$(ps -A | grep -e "$prog")"
        if [ -z "$prog_ps" ]; then
            printf 'Not running the progress "%s" in background.\n' "$prog"
            printf 'Thus, no reason to connect to VPN again!\n'
            exit 0
        else
            printf '%s is running... trying to reconnect to VPN.\n' "$prog"
        fi
    fi

    if [ -n "$("${VPN}" state | grep -i 'Disconnected')" ]; then
        if [ -x "$(command -v base64)" ]; then
            declare -a arr=()
            i=0
            while IFS= read -r line; do
                array[i]="$line"
                let "i++"
            done <"$VPN_CREDS"
            tmp_file="$(mktemp)"
            # echo "" | tee "$tmp_file" > /dev/null
            echo "${array[0]}" | tee -a "$tmp_file" > /dev/null
            echo "${array[1]}" | base64 --decode | tee -a "$tmp_file" > /dev/null
            echo "${array[2]}" | tee -a "$tmp_file" > /dev/null
            "$VPN" -s connect "${VPN_SERVER}" <"$tmp_file" || \
                printf 'VPN Connection failed due to MFA authentication failure!!\n'
            rm -rf "$tmp_file"
            exit 0
        else
            "$VPN" -s connect "${VPN_SERVER}" <"$HOME/.vpn_creds" || \
                printf 'VPN Connection failed due to MFA authentication failure!!\n'; exit 1
        fi
    else
        printf 'It seems we are still on the VPN!\n'
        exit 0
    fi
}

connect_ucsc_vpn && sleep 1.0 && restart_realvnc_server
