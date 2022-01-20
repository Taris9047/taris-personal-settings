#!/bin/sh

if [ ! -x "$(command -v rclone)" ]; then
    printf 'rclone cannot be found! exiting...\n'
    exit
fi

# RClone command
RClone="$(command -v rclone)"

# Google drive location
GOOGLE_DRIVE="/home/$(/usr/bin/whoami)/.google-drive"

# Onedrive location
ONEDRIVE="/home/$(/usr/bin/whoami)/.onedrive"

# Mounting Google Drive
if grep -qs "$GOOGLE_DRIVE" '/proc/mounts'; then
	sleep 2
elif [ ! -f "$HOME/.config/rclone/rclone.conf" ]; then
	sleep 2
else
    if [ ! -z "$(grep -i "\[google-drive\]" "/home/$(/usr/bin/whoami)/.config/rclone/rclone.conf")" ]; then
		${RClone} mount google-drive: "$GOOGLE_DRIVE" &
		sleep 2
	fi
fi

sleep 2

# Mounting MS Onedrive
if grep -qs "$ONEDRIVE" '/proc/mounts'; then
    sleep 2
elif [ ! -f "/home/$(/usr/bin/whoami)/.config/rclone/rclone.conf" ]; then
    sleep 2
else
    if [ ! -z "$(grep -i "\[onedrive\]" "/home/$(/usr/bin/whoami)/.config/rclone/rclone.conf")" ]; then
        ${RClone} mount --vfs-cache-mode writes onedrive: "$ONEDRIVE" &
        sleep 2
    fi
fi

printf 'Cloud drives are all mounted!!\n'
