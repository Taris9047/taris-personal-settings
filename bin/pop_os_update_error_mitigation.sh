#!/usr/sh

printf 'When we have problems with Pop! OS'' package manger...\n' 

printf 'When PackageKit daemon disappeared...\n'
sudo dpkg --configure -a && \
  sudo apt update && \
  sudo apt -y upgrade



