#!/bin/bash

unalias vncstart 2> /dev/null
unalias vnckill 2> /dev/null

if [ -x "$(command -v vncstart)" ]; then
  printf 'Xtightvnc is not found!\n'
  exit 1
fi

# ports to ignore: 5901-10


# Tightvnc case

# Preferred port: 5907
vncport=7

vnc_port_select_Xtightvnc() {
    all_vnc_servers="$(ps -ef | grep 'Xtightvnc' | awk 'match($0, / :[0-9]+/) {print substr( $0, RSTART, RLENGTH)}')"
    my_vnc_server="$(ps -ef | grep 'Xtightvnc' | awk 'match($0, / :[0-9]+/) {print substr( $0, RSTART, RLENGTH)}')"

}

vncstart() {
    vncserver :${vncport} -geometry 1760x990 -depth 24
}

vnckill() {
    vncserver -kill :$vncport
}

vncstart()

printf 'vncserver is set to open at 59%02d\n' "${vncport}"

