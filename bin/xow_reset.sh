#!/bin/sh

die () {
  printf 'ERROR: %s\n' "$1"
  exit 1
}

[ ! -x "$(command -x xow)" ] && die 'xow does not exist on this system!'

printf 'Resetting the XBOX ONE wireless dongle...\n'

sudo systemctl stop xow
sudo systemctl start xow
sudo systemctl kill -s SIGUSR1 xow

printf 'You can now re-pair your XBOX ONE controller now...\n'

