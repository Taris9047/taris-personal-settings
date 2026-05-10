#!/bin/bash -e
# 
# Installs rclone mounting with systemd: No more shell login needed...
#

cwd="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

rclone_conf="${HOME}/.config/rclone/rclone.conf"

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

function enable_rclone_service () {
  local service_name="$1"
  if [[ -n "$2" ]]; then
    local service_name_user_friendly="$2"
  else
    local service_name_user_friendly="${service_name}"
  fi

  local service_fname="rclone@${service_name}.service"

  if [ ! -z "$(grep -i "\[${service_name}\]" "${rclone_conf}")" ]; then
    if [ ! -L "${user_systemd_dir}/rclone@${service_name}.service" ]; then
      printf 'Enabling %s\n' "${service_name_user_friendly}"
      systemctl --user enable --now rclone@${service_name}.service
    else
      printf '%s is already enabled!!\n' "${service_name_user_friendly}"
    fi
  fi
}


# Currently, we have google-drive and onedrive
#
# Google Drive
enable_rclone_service 'google-drive' 'GoogleDrive'
# OneDrive
enable_rclone_service 'onedrive' 'MS Onedrive'
# Added iCloudDrive recently
# iCloud Drive
enable_rclone_service 'icloud' 'iCloudDrive'

printf 'RClone Systemd Integration Enabled!!\n'

