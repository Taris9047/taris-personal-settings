#!/bin/bash

# Dry run?
DRY_RUN=false

# usage display
usage() {
    echo "Usage: $0 [-d]"
    echo "  -d    Dry run (show commands without executing)"
    exit 1
}

# Parse flags
while getopts "d" opt; do
	case $opt in
		d) DRY_RUN=true ;;
		*) usage ;;
	esac
done

# run command method
run_cmd() {
	local cmd="$1"
	local msg="$2"
	if [ "$DRY_RUN" == true ]; then
		echo "[DRY-RUN] $cmd"
	else
		if eval "$cmd"; then
			printf '✅ %s\n' "$msg"
		else
			printf '❌ Error: Failed to execute: %s\n' "$cmd"
		fi
	fi
}

# 1. Root Privilege Check
# We allow dry-run even if not root, but for real changes, we insist on sudo.
if [ "$DRY_RUN" = false ] && [ "$EUID" -ne 0 ]; then
    printf "❌ Error: This script must be run with root privileges (use sudo).\n"
    exit 1
fi

# 2. Read in the configuration
NVIDIA_LIMITS_FILE='/etc/nvidia-limits.conf'
if [ ! -f "${NVIDIA_LIMITS_FILE}" ]; then
	printf 'nvidia-limits.conf is not installed yet!\n'
	exit 1
fi
source "${NVIDIA_LIMITS_FILE}"

# 3. Check nvidia-smi status
if [ ! -x "$(command -v nvidia-smi)" ]; then
	printf '❌ Error: nvidia-smi does not exist in the system!\nCheck NVIDIA Drivers!\n'
	exit 1
fi

if [ -z "$(nvidia-smi -L | grep -iw UUID)" ]; then
	printf '❌ Error: nvidia-smi does not show GPU UUIDs.\nPlease check driver installation status!\n'
	exit 1
fi

# 4. Now do the NVIDIA thing...
echo "--- NVIDIA Power Limit Setup ---"
if [ "$DRY_RUN" = true ]; then 
	echo "Mode: DRY-RUN (No changes will be made)"; 
else
	echo "Mode: Live (Applying Changes!)";
fi

#  1) persistence mode ON
run_cmd "nvidia-smi -pm 1" "NVIDIA Persistence Mode: ON"
#  2) Set power limits for GPU 0 and 1
if [ -n "${LIMIT_0}" ]; then
	run_cmd "nvidia-smi -i 0 -pl ${LIMIT_0}" "nvidia-smi: Power limit for GPU 0 set to ${LIMIT_0}W"
fi

if [ -n "${LIMIT_1}" ]; then
	run_cmd "nvidia-smi -i 1 -pl ${LIMIT_0}" "nvidia-smi: Power limit for GPU 1 set to ${LIMIT_1}W"
fi

echo "--------------------------------"

