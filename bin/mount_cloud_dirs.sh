#!/bin/sh -e

# Mounts cloud directories once the are set up. This script needs rclone to function properly.

die () {
    printf 'ERROR: %s\n' "%1"
    exit 1
}

# DIE with honor if rclone doesn't exists!!
[ ! -x "$(command -v rclone)" ] && die "Yikes! we need rclone to mount any cloud directories!!"

# We currently has GoogleDrive and Onedrive. See if they are set up and if they are set up, mount them
# when loggin in!

# Default cloud mount positions
GOOGLE_DRIVE="$HOME/.google-drive"
ONE_DRIVE="$HOME/.onedrive"

# Google Drive
if [ -x "$(command -v rclone)" ]; then

	if [ ! -d "$GOOGLE_DRIVE" ]; then
		# printf "%b Google drive mount point not found! making one..\n" "${check_symbol}"
		mkdir -pv "$GOOGLE_DRIVE"
	fi

	if grep -qs "$GOOGLE_DRIVE" '/proc/mounts'; then
		# printf "%b Google Drive already mounted at $GOOGLE_DRIVE\n" "${check_symbol}"
        sleep 0.1
	elif [ ! -f "$HOME/.config/rclone/rclone.conf" ]; then
		# printf "%b RClone was for Google drive not set up yet!\n" "${check_symbol}"
        sleep 0.1
	else
		if [ ! -z "$(grep -i "\[google-drive\]" "$HOME/.config/rclone/rclone.conf")" ]; then
			printf '%b Mounting Google Drive to %s\n' "${check_symbol}" "${GOOGLE_DRIVE}"
			rclone mount google-drive: "$GOOGLE_DRIVE" &
			sleep 2
		fi
	fi
fi

# Microsoft Onedrive
if [ -x "$(command -v rclone)" ]; then
    if grep -qs "$ONE_DRIVE" '/proc/mounts'; then
        # printf "%b MS One Drive already mounted at $ONE_DRIVE\n" "${check_symbol}"
        sleep 0.1
    elif [ ! -f "$HOME/.config/rclone/rclone.conf" ]; then
        # printf "%b RClone for Onedrive was not set up yet!\n" "${check_symbol}"
        sleep 0.1
    else
        if [ ! -z "$(grep -i "\[onedrive\]" "$HOME/.config/rclone/rclone.conf")" ]; then
            # printf "%b Mounting MS One Drive to %s\n" "${check_symbol}" "${ONE_DRIVE}"
            rclone mount --vfs-cache-mode writes onedrive: "$ONE_DRIVE" &
            sleep 2
        fi
    fi
fi
