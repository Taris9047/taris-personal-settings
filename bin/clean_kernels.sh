#!/bin/bash

if [ -x "$(command -v dpkg)" ]; then
  sudo dpkg -l 'linux-*' | sed '/^ii/!d;/'"$(uname -r | sed "s/\(.*\)-\([^0-9]\+\)/\1/")"'/d;s/^[^ ]* [^ ]* \([^ ]*\).*/\1/;/[0-9]/!d' | xargs sudo apt-get -y purge
fi

if [ -x "$(command -v dnf)" ]; then
  sudo dnf -y remove $(dnf repoquery --installonly --latest-limit=-1 -q)
fi

