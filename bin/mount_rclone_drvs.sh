#!/bin/sh

if [ ! -x "$(command -v rclone)" ]; then
    printf 'rclone cannot be found! exiting...\n'
    exit
fi


RClone="($command -v rclone)"

# Mounting Google Drive
${RClone} mount google-drive: /home/taris/.google-drive

sleep 1.2

# Mounting MS Onedrive
${RClone} mount --vfs-cache-mode writes onedrive: /home/taris/.onedrive

sleep 1.2

printf 'Cloud drives are all mounted!!\n'
