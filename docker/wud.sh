#!/bin/bash

# Prompt for credentials
read -p "Enter WUD Admin Username [admin]: " admin_user
admin_user=${admin_user:-admin}

# -s makes the password input silent (hidden)
read -sp "Enter WUD Admin Password: " admin_password
echo "" # Add a newline after hidden input

if [[ -z "$admin_password" ]]; then
    echo "Error: Password cannot be empty."
    exit 1
fi

# Check if a container named 'wud' already exists
if [ "$(docker ps -aq -f name=^wud$)" ]; then
    echo "Container 'wud' already exists."
    read -p "Do you want to remove the existing 'wud' container first? (y/n) " confirm
    if [[ $confirm == [yY] ]]; then
        echo "Removing existing container..."
        docker rm -f wud
    else
        echo "Aborting. Please stop or remove the existing container manually."
        exit 1
    fi
fi

echo "Starting WUD container..."

# Execute the docker command
docker run -d \
  --name wud \
  -e WUD_AUTH_ADMIN_USER="$admin_user" \
  -e WUD_AUTH_ADMIN_PASSWORD="$admin_password" \
  -v "/var/run/docker.sock:/var/run/docker.sock" \
  -p 3000:3000 \
  getwud/wud

if [ $? -eq 0 ]; then
    echo "--------------------------------------------------"
    echo "WUD is running successfully!"
    echo "Access it at: http://localhost:3000"
    echo "--------------------------------------------------"
else
    echo "Failed to start WUD container."
    exit 1
fi

