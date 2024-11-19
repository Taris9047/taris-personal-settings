#!/bin/bash -e
# 
# Installs rclone mounting with systemd: No more shell login needed...
#

cwd="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

if [ ! -x "$(command -v rclone)" ]; then
  printf 'rclone is not installed in the system!! Quitting!!\n'
  exit 1
fi

if [ ! -x "$(command -v systemctl)" ]; then
  printf 'Looks like the system is lacking the Systemd??\n'
  exit 1
fi

# User systemd definition directory
user_systemd_dir="${HOME}/.config/systemd/user"
mkdir -p "${user_systemd_dir}"

# Making symbolic links to user systemd directory...
ln -sfv "${cwd}/rclone@.service" "${user_systemd_dir}/rclone@.service"
find "${cwd}" -iname '*.env' -exec ln -sfv '{}' "${user_systemd_dir}" \;

# Currently, we have google-drive and onedrive
#
# Google Drive
if [ ! -z "$(grep -i "\[google-drive\]" "${HOME}/.config/rclone/rclone.conf")" ]; then
  printf 'Enabling GoogleDrive\n'
  systemctl --user enable --now rclone@google-drive.service
fi
# OneDrive
if [ ! -z "$(grep -i "\[onedrive\]" "${HOME}/.config/rclone/rclone.conf")" ]; then
  printf 'Enabling OneDrive\n'
  systemctl --user enable --now rclone@onedrive.service
fi

printf 'RClone Systemd Integration Enabled!!\n'

