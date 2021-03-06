#!/bin/sh

PIP=$(command -v pip3)

if [ $# -eq 0 ]; then
    echo "Setting up PIP as ... " $PIP
else
    PIP=$1
    echo "PIP will be ... " $PIP
fi

$PIP list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 $PIP install -U

$PIP freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 $PIP install -U


