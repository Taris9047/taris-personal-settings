#!/bin/sh -e
#
# A Rclone based cloud drive backup script.
# Prepared for Leila. A main server.
#
# Add this script to any cron entry to ensure this backup happens automatically.
#


die () {
  printf 'ERROR: %s\n' "$1"
  exit 1
}

if [ ! -x "$(command -v rclone)" ]; then
  die 'Ah crap, we need rclone!!'
fi

RClone=$(which rclone)
BackupRoot="/data/"
BackupDestGoogle="$BackupRoot"/GoogleDrive
BackupDestMSOne="$BackupRoot"/OneDrive

# Backing up GoogleDrive
if [ -d "$BackupDestGoogle" ]; then
	printf 'Backing up GoogleDrive!!\n'
	${RClone} copy --update --verbose \
		--transfers 30 --checkers 8 --contimeout 60s --timeout 300s \
		--retries 5 --low-level-retries 10 --stats 1s \
		 "google-drive:" "$BackupDestGoogle"
fi

if [ -d "$BackupDestMSOne" ]; then
	printf 'Backing up Onedrive!!\n'
	${RClone} copy --update --verbose \
		--transfers 30 --checkers 8 --contimeout 60s --timeout 300s \
		--retries 5 --low-level-retries 10 --stats 1s \
		 "onedrive:" "$BackupDestMSOne"
fi

printf 'Backup done!!\n'
printf '\n'



