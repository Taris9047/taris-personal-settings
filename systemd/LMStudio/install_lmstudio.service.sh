#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Define paths
SOURCE_SERVICE="${SCRIPT_DIR}/lmstudio.service"
TARGET_DIR="$HOME/.config/systemd/user"
TARGET_SERVICE="${TARGET_DIR}/lmstudio.service"
ENV_FILE="$HOME/.lms_model.env"
LMS_BIN="$HOME/.lmstudio/bin/lms"

echo "⚙️ Starting LM Studio user service installation..."

# 1. Print locally installed models for reference
if [ -f "$LMS_BIN" ]; then
    echo -e "\n📦 Fetching locally installed LM Studio models..."
    # Wake up daemon briefly if it's currently stopped to fetch the list
    $LMS_BIN daemon up --background 2>/dev/null || true
    sleep 1.5
    
    # Check if any models exist
    if $LMS_BIN ls 2>/dev/null | grep -q -v "No models"; then
        echo "--------------------------------------------------------"
        $LMS_BIN ls
        echo "--------------------------------------------------------"
        echo "💡 Use one of the identifiers above for your LM_MODEL variable."
    else
        echo "⚠️ No locally downloaded models found. You can add them later using 'lms get <model>'."
    fi
    echo ""
else
    echo "ℹ️ Note: 'lms' CLI binary not found at $LMS_BIN. Skipping local model list preview."
fi

# 2. Verify that your service file exists
if [ ! -f "$SOURCE_SERVICE" ]; then
    echo "❌ Error: Source service file not found at $SOURCE_SERVICE"
    exit 1
fi

# 3. Check, create, or update the environment file
if [ ! -f "$ENV_FILE" ]; then
    echo "📄 Creating default environment file at $ENV_FILE..."
    cat << EOF > "$ENV_FILE"
# LM Studio Configurations
LMS_SERVER_HOST=127.0.0.1
LMS_SERVER_PORT=1234
LM_MODEL=
LM_CONTEXT=2048
EOF
else
    echo "📝 Found existing environment file. Appending missing LMS settings..."
    # Ensure LMS_SERVER_HOST exists in the file, if not, append it
    if ! grep -q "^LMS_SERVER_HOST=" "$ENV_FILE"; then
        echo "LMS_SERVER_HOST=127.0.0.1" >> "$ENV_FILE"
    fi
    # Ensure LMS_SERVER_PORT exists in the file, if not, append it
    if ! grep -q "^LMS_SERVER_PORT=" "$ENV_FILE"; then
        echo "LMS_SERVER_PORT=1234" >> "$ENV_FILE"
    fi
fi

# 4. Create the standard systemd user config directory if it doesn't exist
mkdir -p "$TARGET_DIR"

# 5. Create a symlink from your custom path to the systemd user directory
echo "🔗 Linking service file to systemd user space..."
if [ -L "$TARGET_SERVICE" ] || [ -f "$TARGET_SERVICE" ]; then
    rm "$TARGET_SERVICE" # Remove existing link/file if re-running
fi
ln -s "$SOURCE_SERVICE" "$TARGET_SERVICE"

# 6. Reload the systemd user daemon
echo "🔄 Reloading systemd user manager configuration..."
systemctl --user daemon-reload

# 7. Enable the service to start at system boot
echo "🔒 Enabling lmstudio.service to run at system boot..."
systemctl --user enable lmstudio.service

# 8. Ensure User Lingering is active for Headless setups
if ! loginctl show-user "$USER" | grep -q "Linger=yes"; then
    echo "⏳ Enabling system user lingering so service boots headless..."
    sudo loginctl enable-linger "$USER"
fi

echo "🎉 Installation complete!"
echo "✨ Start it now with: systemctl --user start lmstudio.service"
echo "📊 Check logs with: journalctl --user -u lmstudio.service -f"
