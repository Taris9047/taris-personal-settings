#!/bin/sh

# Installs packages for Linux

#
# Ubuntu 
#
runUbuntu() {
  apt-get update && apt-get install -y libc6-dev-i386 net-tools graphviz make unzip zip g++ libtinfo5 xvfb git libncursesw5 libc6-dev-i386
}

#
# RHEL
#
runCentOSnRHELCommon() {
  dnf install -y graphviz redhat-lsb openssl libXScrnSaver xorg-x11-utils gcc gcc-c++ git kernel-headers-"$(uname -r)" kernel-devel-"$(uname -r)" Xvfb
}

runCentOS() {
  dnf install -y epel-release 
}

runRHEL() {
  LIBTINFO=$(find "/lib64/" -type f -name "libtinfo.so*")
  ln -sfv ${LIBTINFO} /lib64/libtinfo.so.5
}

#
# Help stuff
help() {
  printf '%s -h\n\tPrints this message.\n' "$0"
  printf '%s <distro_name_in_lowercase>\n\tTries to install packages of the given distro.\n' "$0"
  printf '\tAt the time, we only support ubuntu, rhel, centos, pop, and mint\n'
  exit 0
}


#
# Main command
#
if [ ! -f "/etc/os-release" ]; then
  printf '/etc/os-release not found. Cannot determine the distro.\n'
  printf 'Aborting... please install packages manually.\n'
  printf '%s -h for info' "$0"
  exit 0
fi
  
printf 'Detecting Linux Distro info...\n'
OSDIST=$(awk -F= '$1=="ID" { print $2 ;}' /etc/os-release)
printf 'ID=%s\n' "${OSDIST}"
OSREL=$(awk -F= '$1=="VERSION_ID" { print $2 ;}' /etc/os-release)
printf 'VERSION_ID=%s\n' "${OSREL}"

if [ "$#" -eq 1 ]; then
  if [ "$1" = "-h" ]; then
    help
  else
    OSDIST="$1"
    print 'Linux Distro given by user: %s\n' "${OSDIST}"
  fi
fi



# Ubuntu and Ubuntu like distros... (including Pop and mint)
if [ "${OSDIST}" = "ubuntu" ] || [ "${OSDIST}" = "pop" ] || [ "${OSDIST}" = "mint" ]; then
  runUbuntu
fi

# CentOS... if anyone still using it...
if [ "${OSDIST}" = "centos" ]; then
  runCentOSnRHELCommon 
  runCentOS
fi

# Old Redhat
if [ "redhat"* = "${OSDIST}" ]; then
  runCentOSnRHELCommon
fi

# The paid customer for IBM - RHEL
# or Almalinux
if [ "rhel"* = "${OSDIST}" ] || [ "almalinux"* = "${OSDIST}" ]; then
  runCentOSnRHELCommon
  runCentOS
fi











