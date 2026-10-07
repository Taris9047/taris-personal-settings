#!/bin/bash

# Check if script is run as root
if [[ $EUID -ne 0 ]]; then
   echo "Error: This script must be run as root (use sudo)."
   exit 1
fi

# Check if username was provided
if [ -z "$1" ]; then
    echo "Usage: sudo ./delete_user_account.sh <username>"
    exit 1
fi

USERNAME=$1
TARGET_DIR="/data/$USERNAME"

echo "--- WARNING: DESTRUCTIVE ACTION ---"
echo "You are about to PERMANENTLY DELETE:"
echo "1. The user account: $USERNAME"
echo "2. The home directory: /home/$USERNAME"
echo "3. The data directory: $TARGET_DIR"
echo "------------------------------------"

# Ask for confirmation
read -p "Are you absolutely sure? (y/N): " confirm
if [[ ! $confirm =~ ^[Yy]$ ]]; then
    echo "Operation cancelled."
    exit 0
fi

# 1. Check if user exists
if ! id "$USERNAME" &>/dev/null; then
    echo "Error: User '$USERNAME' does not exist. Nothing to remove."
    exit 1
fi

# 2. Remove the user and their home directory
echo "Removing user '$USERNAME' and their home directory..."
userdel -r "$USERNAME" 2>/dev/null
if [ $? -eq 0 ] || [ ! -d "/home/$USERNAME" ]; then
    echo "User and home directory removed successfully."
else
    echo "Warning: User removed, but /home/$USERNAME might still exist. Check manually."
fi

# 3. Remove the directory in /data
if [ -d "$TARGET_DIR" ]; then
    echo "Removing data directory: $TARGET_DIR..."
    rm -rf "$TARGET_DIR"
    if [ $? -eq 0 ]; then
        echo "Directory $TARGET_DIR removed successfully."
    else
        echo "Error: Failed to remove $TARGET_DIR. It might be in use or have immutable flags."
        exit 1
    fi
else
    echo "Note: Directory $TARGET_DIR did not exist, skipping."
fi

echo "--- Cleanup Complete ---"

