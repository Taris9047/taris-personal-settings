#!/bin/sh

# Run this script on every boot if you're using crap laptop which handles throttling down too aggressively.

# Make sure those packages are installed
# msr-tool, cpufrequtils
# --> sudo apt install msr-tool cpufrequtils

# Be sure run this script as root

set -u

sudo modprobe msr
# sudo touch temp
r=`sudo /usr/sbin/rdmsr 0x1FC`
#echo $r > temp
s='0x'$r'' 
f=$(($s&0xFFFFE))
sudo /usr/sbin/wrmsr 0x1FC "obase=16;$f"|bc
echo "$r"" write to ""reg 0x1FC" 
echo "BD PROCHOT off."

