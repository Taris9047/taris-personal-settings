#!/bin/bash

# Update the package lists
apt update && apt upgrade -y

# Check if a reboot is needed
if [ -f /var/run/reboot-required ]; then
  wall -n "System will be reboot after 5 minutes for update."
  /usr/bin/sleep 300
  /usr/sbin/reboot
else
  wall -n "System packages updated."
fi
