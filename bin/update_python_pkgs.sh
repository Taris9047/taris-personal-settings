#!/bin/sh

PIP=`which pip3`

if [ $# -eq 0 ]; then
    echo "Setting up PIP as ... " `which pip3`
else
    PIP=$1
    echo "PIP will be ... " $PIP
fi

$PIP list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 $PIP install -U




