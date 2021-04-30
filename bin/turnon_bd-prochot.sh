#!/bin/sh

# Run this script on every boot if you're using crap laptop which handles throttling down too aggressively.

# Make sure those packages are installed
# msr-tool, cpufrequtils
# --> sudo apt install msr-tool cpufrequtils

# Be sure run this script as root

set -u

/sbin/modprobe msr
/usr/sbin/wrmsr 0x1FC 4005d
echo "BD PROCHOT on."

