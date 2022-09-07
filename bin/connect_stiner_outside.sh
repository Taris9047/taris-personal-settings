#!/bin/bash

HOSTNAME='taris9047.ddns.net'
STEINER_LOCAL_ADDR='192.168.0.15'

if [ ! -z "$1" ]; then
	HOSTNAME="$1"
fi

ssh -t taris@$HOSTNAME "ssh taris@$STEINER_LOCAL_ADDR"

