#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Define paths
SOURCE_SERVICE="${SCRIPT_DIR}/lmstudio.service"
TARGET_DIR="$HOME/.config/systemd/user"
TARGET_SERVICE="${TARGET_DIR}/lmstudio.service"
ENV_FILE="$HOME/.lms_model.env"

echo "⚙️ Starting LM Studio user service installation..."

# 1. Verify that your service file exists
if [ ! -f "$SOURCE_SERVICE" ]; then
    echo "❌ Error: Source service file not found at $SOURCE_SERVICE"
    exit 1
fi

# 2. Verify that the environment file exists
if [ ! -f "$ENV_FILE" ]; then
    echo "❌ Error: Environment file not found at $ENV_FILE"
    echo "Please make an environment file looks like this: "
    echo "LM_MODEL=<some_model>"
    echo "LM_CONTEXT=<some_context_size>"
    exit 1
fi

# 3. Create the standard systemd user config directory if it doesn't exist
mkdir -p "$TARGET_DIR"

# 4. Create a symlink from your custom path to the systemd user directory
echo "🔗 Linking service file to systemd user space..."
if [ -L "$TARGET_SERVICE" ] || [ -f "$TARGET_SERVICE" ]; then
    rm "$TARGET_SERVICE" # Remove existing link/file if re-running
fi
ln -s "$SOURCE_SERVICE" "$TARGET_SERVICE"

# 5. Reload the systemd user daemon
echo "🔄 Reloading systemd user manager configuration..."
systemctl --user daemon-reload

# 6. Enable the service to start at system boot
echo "🔒 Enabling lmstudio.service to run at system boot..."
systemctl --user enable lmstudio.service

echo "🎉 Installation complete!"
echo "✨ Start it now with: systemctl --user start lmstudio.service"
echo "📊 Check logs with: journalctl --user -u lmstudio.service -f"
