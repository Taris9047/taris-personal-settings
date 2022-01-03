#!/bin/sh

# This script enables or disables touchscreen one finger scroll on Firefox... for 2-in-1 laptops..

# The sed command was adopted from ...
# https://askubuntu.com/a/979384 

if [ ! -x "$(command -v firefox)" ]; then
  printf 'Well, we need firefox to be installed to begin with...\n'
  exit 1
fi

if [ -z $FFDesktopIcon ]; then
  FFDesktopIcon='/usr/share/applications/firefonx.desktop'
fi

if [ ! -f "$FFDesktopIcon" ]; then
  printf 'O boy... we need to define where the firefox.desktop is located!!\n'
  printf 'Run the script again with ... env FFDesktopIcon=<firefox.desktop path>\n'
  exit 1
fi

if [ -z "$(grep 'MOZ_USE_XINPUT2=1' $FFDesktopIcon)" ]; then
  sudo sed -i "s|Exec=|Exec=env MOZ_USE_XINPUT2=1 |g" /usr/share/applications/firefox.desktop
  printf 'Enabled one finger scroll for Firefox! Restart the system or re-login to fully apply the feature!!\n'
else
  sudo sed -i "s|Exec=env MOZ_USE_XINPUT2=1 |Exec=|g" /usr/share/applications/firefox.desktop
  printf 'Disabled one finger scroll for Firefox! Restart the system or re-login to fully apply the feature!!\n'
fi

