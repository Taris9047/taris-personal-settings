#!/usr/bin/env bash

unalias vncstart 2> /dev/null
unalias vnckill 2> /dev/null


# ports to ignore: 5901-10


# Tightvnc case

# Preferred port: 5903
vncport=3

vnc_port_select_Xtightvnc() {
    all_vnc_servers="$(ps -ef | grep 'Xtightvnc' | awk 'match($0, / :[0-9]+/) {print substr( $0, RSTART, RLENGTH)}')"
    my_vnc_server="$(ps -ef | grep 'Xtightvnc' | awk 'match($0, / :[0-9]+/) {print substr( $0, RSTART, RLENGTH)}')"
  

}




vncstart() {
    vncserver :$vncport -geometry 1600x900 -depth 24
}
vnckill() {
    vncserver -kill :$vncport
}
printf 'vncserver is set to open at 59%02d\n' $vncport

