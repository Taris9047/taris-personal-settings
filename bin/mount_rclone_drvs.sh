#!/bin/sh

if [ ! -x "$(command -v rclone)" ]; then
    printf 'rclone cannot be found! exiting...\n'
    exit
fi

RClone="$(command -v rclone)"

# Mounting Google Drive
if [ -x "$(command -v rclone)" ]; then

	if grep -qs "$GOOGLE_DRIVE" '/proc/mounts'; then
		sleep 1.25
	elif [ ! -f "$HOME/.config/rclone/rclone.conf" ]; then
	  sleep 1.25
  else
		if [ ! -z "$(grep -i "\[google-drive\]" "$HOME/.config/rclone/rclone.conf")" ]; then
			${RClone} mount google-drive: "$GOOGLE_DRIVE" &
			sleep 2
		fi
	fi
fi

sleep 1.2

# Mounting MS Onedrive
${RClone} mount --vfs-cache-mode writes onedrive: /home/taris/.onedrive

if grep -qs "$ONE_DRIVE" '/proc/mounts'; then
    sleep "${line_delay}"
elif [ ! -f "$HOME/.config/rclone/rclone.conf" ]; then
    sleep 1.25
else
    if [ ! -z "$(grep -i "\[onedrive\]" "$HOME/.config/rclone/rclone.conf")" ]; then
        ${RClone} mount --vfs-cache-mode writes onedrive: "$ONE_DRIVE" &
        sleep 2
    fi
fi

printf 'Cloud drives are all mounted!!\n'
