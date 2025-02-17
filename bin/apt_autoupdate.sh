#!/bin/bash

#
# Ubuntu / Debian Linux package manager update script.
#
# --> Usually, those distributions shipped with a GUI autoupdate interface.
# But they do not work out exactly every time. So, let's just cron the
# operation with this file.
#

# Update the package lists
apt update && apt upgrade -y

# Check if a reboot is needed
if [ -f /var/run/reboot-required ]; then
  /usr/bin/wall -n "System will be reboot after 5 minutes for update."
  /usr/bin/sleep 300
  if [ -z "$(ps -A | grep -E 'swb|sdevice|sde')" ]; then
    /usr/sbin/reboot
  elif
    /usr/bin/wall -n "It seems someone is using Sentaurus. Exiting. Be sure to reboot the system ASAP."
  fi
else
  /usr/bin/wall -n "System packages updated."
fi
