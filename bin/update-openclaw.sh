#!/usr/bin/env bash
set -euo pipefail

# 1. Load NVM for this script session
export NVM_DIR="$HOME/.nvm"
if [ -s "$NVM_DIR/nvm.sh" ]; then
    \. "$NVM_DIR/nvm.sh" --no-use
else
    echo "Error: nvm.sh not found in $NVM_DIR" >&2
    exit 1
fi

USER_SERVICE="$HOME/.config/systemd/user/openclaw-gateway.service"
SYSTEM_SERVICE="/etc/systemd/system/openclaw-gateway.service"

# 2. Detect the previous Node version from the systemd service or symlink
OLD_VER=""
if [ -f "$USER_SERVICE" ]; then
    OLD_VER=$(grep -oE 'versions/node/v[0-9]+\.[0-9]+\.[0-9]+' "$USER_SERVICE" | head -n1 | awk -F'/' '{print $3}' || true)
elif [ -f "$SYSTEM_SERVICE" ]; then
    OLD_VER=$(grep -oE 'versions/node/v[0-9]+\.[0-9]+\.[0-9]+' "$SYSTEM_SERVICE" | head -n1 | awk -F'/' '{print $3}' || true)
fi
if [ -z "$OLD_VER" ] && [ -L "$NVM_DIR/current" ]; then
    OLD_VER=$(basename "$(readlink -f "$NVM_DIR/current")")
fi

# 3. Install the newest LTS Node version cleanly (no package migration)
LTS_VER=$(nvm version-remote --lts)
echo "Previous Node: ${OLD_VER:-none} | Target LTS Node: $LTS_VER"

nvm install "$LTS_VER"
NEW_VER=$(nvm current)
nvm alias default "$NEW_VER"
ln -sfn "$NVM_DIR/versions/node/$NEW_VER" "$NVM_DIR/current"

# 4. If staying on the same LTS version, uninstall old openclaw first; then fresh install
if [ "$OLD_VER" = "$NEW_VER" ]; then
    echo "Uninstalling previous OpenClaw on $NEW_VER..."
    npm uninstall -g openclaw || true
fi

echo "Installing latest OpenClaw on Node $NEW_VER..."
npm install -g openclaw@latest

# 5. Update systemd unit paths and restart the service
if [ -f "$USER_SERVICE" ]; then
    sed -i -E "s|/versions/node/v[0-9]+\.[0-9]+\.[0-9]+/|/versions/node/$NEW_VER/|g" "$USER_SERVICE"
    systemctl --user daemon-reload
    systemctl --user restart openclaw-gateway.service
    echo "Restarted user service: openclaw-gateway.service"
elif [ -f "$SYSTEM_SERVICE" ]; then
    sudo sed -i -E "s|/versions/node/v[0-9]+\.[0-9]+\.[0-9]+/|/versions/node/$NEW_VER/|g" "$SYSTEM_SERVICE"
    sudo systemctl daemon-reload
    sudo systemctl restart openclaw-gateway.service
    echo "Restarted system service: openclaw-gateway.service"
fi

# 6. Uninstall the old Node version (deletes old Node + its old openclaw node_modules)
if [ -n "$OLD_VER" ] && [ "$OLD_VER" != "$NEW_VER" ] && [ -d "$NVM_DIR/versions/node/$OLD_VER" ]; then
    echo "Removing old Node version $OLD_VER and its global packages..."
    nvm uninstall "$OLD_VER"
fi

echo "Update complete! OpenClaw is freshly installed on LTS Node $NEW_VER."
