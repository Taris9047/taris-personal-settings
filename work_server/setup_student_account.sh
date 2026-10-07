#!/bin/bash

#
# User generation script for student users
#
#
#
# Check if script is run as root
if [[ $EUID -ne 0 ]]; then
   echo "Error: This script must be run as root (use sudo)."
   exit 1
fi

# Check if username was provided
if [ -z "$1" ]; then
    echo "Usage: sudo ./setup_student_account.sh <username>"
    exit 1
fi

USERNAME=$1
TARGET_DIR="/data/$USERNAME"
MOUNTPOINT="/data"
USER_PASSWORD="ufsd-cad"

echo "--- Starting User Setup for: $USERNAME ---"

# 1. Check if /data exists
if [ ! -d "$MOUNTPOINT" ]; then
    echo "Error: $MOUNTPOINT does not exist. Please ensure your drive is mounted."
    exit 1
fi

# 2. Create the user account
# -m: Creates a home directory
# -s: Sets the default shell to bash
if id "$USERNAME" &>/dev/null; then
    echo "Error: User '$USERNAME' already exists."
    exit 1
else
    echo "Creating user account: $USERNAME..."
    useradd -m -s /bin/bash "$USERNAME"
    echo "User '$USERNAME' created successfully."
fi

# 2.5. Set user's account password to ufsd-cad for temporary usage.
echo "Setting password for $USERNAME..."
echo "$USERNAME:$USER_PASSWORD" | chpasswd
if [ $? -eq 0 ]; then
    echo "Password set successfully."
else
    echo "Error: Failed to set password."
    exit 1
fi

# 3. Create the dedicated directory in /data
echo "Creating directory: $TARGET_DIR..."
mkdir -p "$TARGET_DIR"

# 4. Set ownership of the /data directory to the new user
echo "Setting ownership of $TARGET_DIR to $USERNAME..."
chown "$USERNAME":"$USERNAME" "$TARGET_DIR"

# 5. Create the symbolic link in the user's home directory
echo "Creating symbolic link in /home/$USERNAME/data..."
ln -s "$TARGET_DIR" "/home/$USERNAME/data"

# 6. Set ownership of the home directory (ensure the user owns their link/home)
# Note: The link itself is in the home dir, but we want the user to own the link target/access.
chown -R "$USERNAME":"$USERNAME" "/data/$USERNAME/data_link"
chmod -R 2775 "/data/$USERNAME"

echo "--- Setup Complete ---"
echo "User: $USERNAME"
echo "Home: /home/$USERNAME"
echo "Data Directory: $TARGET_DIR"
echo "Symlink: /home/$USERNAME/data_link -> $TARGET_DIR"

